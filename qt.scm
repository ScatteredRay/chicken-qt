(module qt *
  (import scheme chicken foreign data-structures extras srfi-1 srfi-69)
  (declare (emit-external-prototypes-first))
  (require-extension srfi-69)
  (import-for-syntax matchable)

  ;; There exists no way to call foreign-type-declaration, so hack it

  (define-for-syntax (foreign-type-decl-hack typesym)
    (string-substitute "-ptr" "*" (->string typesym)))

  ;; Macros need hygiene!

  (define-record-type qt-class-type
    (make-qt-class-type name parent message-table)
    qt-class-type?
    (name qt-class:get-name)
    (parent qt-class:get-parent)
    (message-table qt-class:get-message-table))

  (define-record-type qt-class
    (make-qt-class type qt-ptr)
    qt-class?
    (type qt-class:get-type)
    (qt-ptr qt-class:get-ptr qt-class:set-ptr!))

  (define (get-class-method type method)
    (hash-table-ref
     (qt-class:get-message-table type)
     method
     (lambda ()
       (if (qt-class:get-parent type)
           (get-class-method
            (qt-class:get-parent type)
            method)
           '()))))

  (define (maybe-get-qt-ptr var)
    (if (qt-class? var)
        (qt-class:get-ptr var)
        var))

  (define (qt-class:set-class-method! class name func)
    (hash-table-set!
     (qt-class:get-message-table class)
     name
     func))

  (define (call-impl method class . params)
    (apply
     (get-class-method (qt-class:get-type class) method)
     (cons class params)))

  ;; Need better diagnostics when call params fail!
  (define-syntax call
    (syntax-rules ()
      ((call method class params ...)
       (call-impl 'method class params ...))))

  (define qt-class-list (make-hash-table))

  (define-for-syntax qt-class:gen-make
    (lambda (class-name self set)
      `(letrec ((,self
                 (make-qt-class
                  (hash-table-ref qt-class-list ',class-name)
                  '())))
         (qt-class:set-ptr!
          ,self
          ,set)
         ,self)))

  (define-for-syntax make-name
    (lambda (i)
      (string->symbol
       (string-append
        "s"
        (->string i)))))

  (define-for-syntax ptr-type-name
    (lambda (sym)
      (symbol-append sym '-ptr)))

  (define-for-syntax ref-type-name
    (lambda (sym)
      (symbol-append sym '-ref)))

  (define-for-syntax param-list
    (lambda (func params i)
      (if (null? params)
          '()
          (let ((param (car params)))
            (cons
             (func param i)
             (param-list func (cdr params) (+ i 1)))))))

  (define-for-syntax map-names
    (lambda (func params)
      (param-list
       (lambda (param i)
         (func param (make-name i)))
       params
       0)))

  (define-for-syntax symbol-append
    (lambda symbol-list
      (string->symbol
       (apply
        string-append
        (map
         (lambda (x)
           (->string x))
         symbol-list)))))

  (define-for-syntax qt-proxy-callback-name
    (lambda (name)
      (string->symbol
       (string-append
        "C_callback_"
        (->string name)))))

  (define-for-syntax qt-proxy-callback
    (lambda (callback-name
             params
             return
             callback)
      `(define-external (,(qt-proxy-callback-name callback-name)
                         (scheme-object self)
                         ,@(map-names
                            (lambda (param name)
                              (list
                               param
                               name))
                            params))
         ,return
         (,callback
          self
          ,@(map-names
             (lambda (param name)
               name)
             params)))))

  (define-syntax qt-foreign-define
    (lambda (e r c)
      (let
          ((class (cadr e))
           (method (caaddr e))
           (self (cadr (caddr e)))
           (params (cddr (caddr e)))
           (return (cadddr e))
           (body (car (cddddr e))))
        `(qt-class:set-class-method!
          (hash-table-ref
           qt-class-list
           ',class)
          ',method
          (lambda (,(cadr self)
                   ,@(map
                      cadr
                      params))
            ((foreign-safe-lambda*
              ,return
              (,self
               ,@params)
              ,body)
             (qt-class:get-ptr ,(cadr self))
             ,@(map
                (lambda (param)
                  (cadr param))
                params)))))))

  (define-syntax qt-define-method
    (lambda (e r c)
      (let ((class (cadr e))
            (method (caaddr e))
            (c-method (caaddr e))
            (params (cdaddr e))
            (return (cadddr e)))
        (if (list? method)
            (begin
              (set! method (car method))
              (set! c-method (cadr c-method))))
        `(qt-foreign-define
          ,class
          (,method
           (c-pointer self)
           ,@(param-list
              (lambda (param i)
                (list
                 param
                 (make-name i)))
              params 0))
          ,return
          ,(apply
            string-append
            `(
              ,(if (eq? return 'void)
                   ""
                   "C_return(")
              "(("
              ,(->string class)
              "*)self)->"
              ,(->string c-method)
              "("
              ,@(param-list
                 (lambda (param i)
                   (string-append
                    (if (> i 0)
                        ", "
                        "")
                    (->string (make-name i))))
                 params 0)
              ,(if (eq? return 'void)
                   ""
                   ")")
              ");"))))))

  (define-syntax qt-static-method
    (lambda (e r c)
      (match
        e
        (('qt-static-method
          class-name
          method-name
          return-type
          arg-types)
         `(define
            ,(symbol-append
              class-name ': method-name)
            (foreign-lambda*
             ,return-type
             ,(param-list
               (lambda (param i)
                 (list
                  param
                  (make-name i)))
               arg-types 0)
             ,(apply
               string-append
               `(
                 ,(if (eq? return-type 'void)
                      ""
                      "C_return(")
                 ,(->string class-name)
                 "::"
                 ,(->string method-name)
                 "("
                 ,@(param-list
                    (lambda (param i)
                      (string-append
                       (if (> i 0)
                           ", "
                           "")
                       (->string (make-name i))))
                    arg-types 0)
                 ")"
                 ,(if (eq? return-type 'void)
                      ""
                      ")")
                 ";"))))))))

  ;; TODO: Create a constructor call that causes the object to be destroyed with the Scheme object.
  (define-syntax qt-class
    (lambda (e r c)
      (match
        e
        (('qt-class class-name constructor-list method-list)

         (set! parent-name '())

         (if (list? class-name)
             (begin
               (set! parent-name (cadr class-name))
               (set! class-name (car class-name))))

         `(begin
            (hash-table-set!
             qt-class-list
             ',class-name
             (make-qt-class-type
              ',class-name
              ,(if (not (null? parent-name))
                   `(hash-table-ref/default qt-class-list ',parent-name #f)
                   #f)
              (make-hash-table)))

            (define-foreign-type
              ,(ptr-type-name class-name)
              (c-pointer (struct ,class-name))
              maybe-get-qt-ptr
              (lambda (qt-ptr)
                ,(qt-class:gen-make
                  class-name
                  (r 'self)
                  'qt-ptr)))

            ,(match
               constructor-list
               ((constructor . constructor-params)
                `(define ,constructor
                   (lambda (,@(delete
                               #f
                               (param-list
                                (lambda (param i)
                                  (if (not (eq? param 'self))
                                      (make-name i)
                                      #f))
                                constructor-params 0)))
                     ;; Can't use the foreign type cohersion functions, because
                     ;; if we need to pass the scheme ptr to C we need a
                     ;; reference before the call.
                     ,(qt-class:gen-make
                       class-name
                       (r 'self)
                       `((foreign-safe-lambda*
                          c-pointer
                          ,(param-list
                            (lambda (param i)
                              (list
                               (if (eq? param 'self)
                                   'scheme-object
                                   param)
                               (make-name i)))
                            constructor-params 0)
                          ,(apply
                            string-append
                            `("C_return(new "
                              ,(->string class-name)
                              "("
                              ,@(param-list
                                 (lambda (param i)
                                   (string-append
                                    (if (> i 0)
                                        ", "
                                        "")
                                    (->string (make-name i))))
                                 constructor-params 0)
                              "));")))
                         ,@(param-list
                            (lambda (param i)
                              (if (eq? param 'self)
                                  (r 'self)
                                  (make-name i)))
                            constructor-params 0))))))
               (()
                ''()))

            ,@(map
               (lambda (method)
                 (match
                   method
                   ((method-name params method-return)
                    `(qt-define-method
                      ,class-name
                      (,method-name ,@params)
                      ,method-return))))
               method-list)
            
            (qt-foreign-define
             ,class-name
             (delete (c-pointer self))
             void
             ,(string-append
               "delete (("
               (->string class-name)
               "*)self);")))))))


  ;; by-ref classes aren't allowed to have parent classes.
  ;; TODO: fix this, QPixmap has a parent.
  (define-syntax qt-ref-class
    (lambda (e r c)
      `(begin
         (qt-class ,@(cdr e))

         ;; This relies on a class having a copy constructor, should ensure this is true.
         ,(let ((class-name (cadr e)))
            `(define-foreign-type
               ,(ref-type-name class-name)
               (ref (struct ,class-name))
               maybe-get-qt-ptr
               (lambda (qt-ptr)
                 ,(qt-class:gen-make
                   class-name
                   (r 'self)
                   `((foreign-lambda* c-pointer (((ref (struct ,class-name)) ref))
                                      ,(string-append
                                        "C_return(new "
                                        (->string class-name)
                                        "(ref)) ;"))
                     qt-ptr))))))))

  (define-syntax qt-proxy-class
    (lambda (e r c)
      (match
        e
        (('qt-proxy-class
          class-name
          parent-class
          (constructor
           constructor-params
           constructor-func)
          parent-params
          destructor-func
          proxies)
         `(begin
            ;; Pre-Constructor callback
            ;; qt-ptr doesn't get set before the constructor is called, so we set it here
            ,(qt-proxy-callback
              (symbol-append
               class-name
               'preconstruct)
              `(,(ptr-type-name
                  class-name))
              'void
              '(lambda (self c-self)
                 (qt-class:set-ptr! self c-self)))
            
            ;; Constructor callback
            ,(qt-proxy-callback
              (symbol-append
               class-name
               'constructor)
              (map
               (lambda (X)
                 (car X))
               constructor-params)
              'void
              constructor-func)

            ;; Destructor callback
            ,(qt-proxy-callback
              (symbol-append
               class-name
               'destructor)
              '()
              'void
              destructor-func)

            ;; Proxies callbacks
            ,@(map
               (lambda (proxy)
                 (apply qt-proxy-callback proxy))
               proxies)
            
            (foreign-declare
             ,(apply
               string-append
               (append
                (list "class " (->string class-name) " : public " (->string parent-class)
                      "{"
                      "	void* proxy_root;"
                      "public:"
                      (->string class-name) "("
                      "C_word proxy")
                (map
                 (lambda (param)
                   (string-append
                    ", "
                    (foreign-type-decl-hack (car param))
                    " "
                    (->string (cadr param))))
                 constructor-params)
                (list
                 ") :"
                 (->string parent-class) "(")
                (map
                 (lambda (params)
                   (->string params))
                 parent-params)
                (list
                 ")"
                 "{"
                 "	proxy_root = CHICKEN_new_gc_root();"
                 "	CHICKEN_gc_root_set(proxy_root, proxy);"
                 (->string (qt-proxy-callback-name
                            (symbol-append
                             class-name
                             'preconstruct)))
                 "(proxy, this);"
                 (->string (qt-proxy-callback-name
                            (symbol-append
                             class-name
                             'constructor)))
                 "(proxy")
                (map
                 (lambda (param)
                   (string-append
                    ", "
                    (->string (cadr param))))
                 constructor-params)
                (list
                 ");"
                 "}"

                 "~" (->string class-name) "()"
                 "{"
                 (->string (qt-proxy-callback-name
                            (symbol-append
                             class-name
                             'destructor)))
                 "(CHICKEN_gc_root_ref(proxy_root));"
                 "CHICKEN_delete_gc_root(proxy_root);"
                 "}")
                
                ;; Proxy method wrappers
                (map
                 (lambda (proxy)
                   (match
                     proxy
                     ((proxy-name params return-type func)
                      (apply
                       string-append
                       (append
                        (list
                         (->string return-type)
                         " "
                         (->string proxy-name)
                         "(")
                        (param-list
                         (lambda (param i)
                           (string-append
                            (if (> i 0)
                                ", "
                                "")
                            (foreign-type-decl-hack param)
                            " "
                            (->string
                             (make-name i))))
                         params
                         0)
                        (list
                         ")"
                         "{"
                         (if (eq? return-type 'void)
                             ""
                             "")
                         (->string
                          (qt-proxy-callback-name proxy-name))
                         "(CHICKEN_gc_root_ref(proxy_root)")
                        (map-names
                         (lambda (param name)
                           (string-append
                            ", "
                            (->string name)))
                         params)
                        (list
                         ");"
                         "}"))))))
                 proxies)
                
                (list "};"))))

            (qt-class
             (,class-name ,parent-class)
             (,constructor
              self
              ,@(map
                 (lambda (param)
                   (car param))
                 constructor-params))
             ()))))))

  (define-syntax qt-app:create-exec-window!
    (lambda (e r c)
      (match
        e
        (('qt-app:create-exec-window!
          create-func
          window-var)
         `(begin
            ;; Define-external doesn't get closure environment,
            ;; needs to be global.
            (define global-window-func #f)
            (define (,create-func window-func)
              (set! global-window-func window-func)
              (define-external (init_window) scheme-object
                (global-window-func))
              (define-external (finalize_window (scheme-object window))
                void
                (call delete window))
              ((foreign-safe-lambda*
                void
                ()
                ,(string-append
                  "int argc = 0;"
                  "char** argv = NULL;"
                  "QApplication a(argc, argv);"
                  "C_word W = init_window();"
                  "a.exec();"
                  "finalize_window(W);")))))))))

  (foreign-declare "#include <QtGui/QApplication>")
  (foreign-declare "#include <QtGui/QMainWindow>")
  (foreign-declare "#include <QtGui/QVBoxLayout>")
  (foreign-declare "#include <QtGui/QPushButton>")
  (foreign-declare "#include <QtUiTools/QUiLoader>")
  (foreign-declare "#include <QtCore/QFile>")
  (foreign-declare "#include <QtGui/QFileDialog>")
  (foreign-declare "#include <QtGui/QGraphicsScene>")
  (foreign-declare "#include <QtGui/QGraphicsView>")
  (foreign-declare "#include <QtGui/QGraphicsPixmapItem>")

  (qt-ref-class
   QByteArray
   ()
   ((data () c-string)))

  (qt-ref-class
   QString
   (make-QString c-string)
   ((toAscii () QByteArray-ref)))

  (qt-class
   QObject
   (make-QObject QObject-ptr)
   ((objectName () QString-ref)
    (setObjectName (QString-ref) void)
    (findChild<QWidget*> (QString-ref) QObject-ptr)))

  (qt-class
   QLayout
   () ())

  (qt-class
   (QWidget QObject)
   (make-QWidget QWidget-ptr)
   ((show () void)
    (setLayout (QLayout-ptr) void)))

  (qt-class
   (QIODevice QObject)
   ()
   ())

  (qt-static-method
   QFileDialog
   getOpenFileName
   QString-ref
   (QWidget-ptr
    QString-ref
    QString-ref
    QString-ref))

  (qt-class
   (QFile QIODevice)
   (make-QFile QString-ref)
   ((open ((enum QIODevice::OpenModeFlag)) bool)
    (close () void)))

  (qt-ref-class
   QPixmap
   (make-QPixmap QString-ref)
   ())

  (qt-class
   QGraphicsPixmapItem
   ()
   ())

  (qt-class
   (QGraphicsScene QObject)
   (make-QGraphicsScene)
   ((addPixmap (QPixmap-ref) QGraphicsPixmapItem-ptr)))

  (qt-class
   (QGraphicsView QWidget)
   ()
   ((setScene (QGraphicsScene-ptr) void)))

  (qt-class
   (QMainWindow QWidget)
   (make-QMainWindow QWidget-ptr)
   ((isAnimated () bool)
    (setCentralWidget (QWidget-ptr) void)
    (centralWidget () QWidget-ptr)))

  (qt-class
   (QUiLoader QObject)
   (make-QUiLoader QObject-ptr)
   ((load (QIODevice-ptr QWidget-ptr) QWidget-ptr)))

  (qt-class
   QPushButton
   (make-QPushButton (ref (struct QString)) (c-pointer (struct QWidget)))
   ())

  (qt-class
   QBoxLayout
   ()
   ((addWidget (QWidget-ptr) void)))

  (qt-class
   (QVBoxLayout QBoxLayout)
   (make-QVBoxLayout QWidget-ptr)
   ())

  (qt-proxy-class
   ImageWindow
   QMainWindow
   (New-ImageWindow
    ((QWidget-ptr parent #f))
    (lambda (self parent)
      (let* ((UIFileLoc (make-QString "imagewindow.ui"))
             (UIFile (make-QFile UIFileLoc))
             (UILoader (make-QUiLoader self))
             (UIWidget (call load UILoader UIFile self))
             (graphicsView (call findChild<QWidget*> UIWidget (make-QString "graphicsView"))))
        (call setObjectName self (make-QString "ImageWindowObject"))
        (print (call data (call toAscii (call objectName self))))
        (print (call data (call toAscii (call objectName graphicsView))))
        (print
         (call data
               (call toAscii
                     (QFileDialog:getOpenFileName
                      self
                      (make-QString "Open Image")
                      (make-QString "")
                      (make-QString "Image Files (*.jpg *.jpeg)")))))
        (call setCentralWidget self UIWidget)
        (call close UIFile)
        (call delete UIFile)
        (call delete UIFileLoc))
      (print "Constructing!")))
   (parent)
   (lambda (self)
     (print "Destructing!"))
   ((SelectImage (bool) void
                 (lambda (self Selected?)
                   (print Selected?)))))

  (qt-app:create-exec-window!
   initialize-qt
   MyImageWindow))