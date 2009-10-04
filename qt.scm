(declare (emit-external-prototypes-first))
(require-extension srfi-1)
(import-for-syntax matchable)

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

(define-syntax call
  (syntax-rules ()
    ((call method class params ...)
     (call-impl 'method class params ...))))

(define qt-class-list (make-hash-table))

(define-for-syntax make-name
  (lambda (i)
    (string->symbol
     (string-append
      "s"
      (->string i)))))

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
                             (cadr param)
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
                (list
                 'maybe-get-qt-ptr
                 (cadr param)))
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
                   (letrec ((,(r 'self)
                             (make-qt-class
                              (hash-table-ref qt-class-list ',class-name)
                              '())))
                     (qt-class:set-ptr!
                      ,(r 'self)
                      ((foreign-safe-lambda*
                        c-pointer
                        ,(param-list
                          (lambda (param i)
                            (list
                             (if (eq? param 'self)
                                 'scheme-object
                                 (cadr param))
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
                                  (if (eq? param 'self)
                                      ""
                                      (string-append
                                       "("
                                       (->string (car param))
                                       ")"))
                                  (->string (make-name i))))
                               constructor-params 0)
                            "));")))
                       ,@(param-list
                          (lambda (param i)
                            (if (eq? param 'self)
                                (r 'self)
                                (list
                                 'maybe-get-qt-ptr
                                 (make-name i))))
                          constructor-params 0)))
                     ,(r 'self)))))
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
            `((,(symbol-append
                 class-name '*)
               c-pointer))
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
                  (->string (caar param))
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
                          (->string (car param))
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

(define-syntax qt-app:exec-window!
  (lambda (e r c)
    (let ((window-var (cadr e))
          (window-constructor (caaddr e))
          (constructor-params (cdaddr e))
          (window-func (cadddr e)))
      `(begin
         (define ,window-var '())
         (define-external (init_window) scheme-object
           (set! ,window-var
                 (,window-constructor ,@constructor-params))
           (,window-func
            ,window-var)
           ,window-var)
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
             "finalize_window(W);")))))))

(foreign-declare "#include <QtGui/QApplication>")
(foreign-declare "#include <QtGui/QMainWindow>")
(foreign-declare "#include <QtGui/QVBoxLayout>")
(foreign-declare "#include <QtGui/QPushButton>")
(foreign-declare "#include <QtUiTools/QUiLoader>")
(foreign-declare "#include <QtCore/QFile>")

(qt-class
 QObject
 (make-QObject (QObject* c-pointer))
 ())

(qt-class
 (QWidget QObject)
 (make-QWidget (QWidget* c-pointer))
 ((show () void)
  (setLayout ((c-pointer (struct QLayout))) void)))

(qt-class
 (QIODevice QObject)
 ()
 ())

(qt-class
 QString
 (make-QString (char* c-string))
 ())

(qt-class
 (QFile QIODevice)
 (make-QFile (QString& (ref (struct QString))))
 ((open ((enum QIODevice::OpenModeFlag)) bool)
  (close () void)))

(qt-class
 (QMainWindow QWidget)
 (make-QMainWindow (QWidget* c-pointer))
 ((isAnimated () bool)
  (setCentralWidget ((c-pointer (struct QWidget))) void)
  (centralWidget () (c-pointer (struct QWidget)))))

(qt-class
 (QUiLoader QObject)
 (make-QUiLoader (QObject* c-pointer))
 ((load ((c-pointer (struct QIODevice))  (c-pointer (struct QWidget))) c-pointer)))

(qt-class
 QPushButton
 (make-QPushButton (QString& (ref (struct QString))) (QWidget* (c-pointer (struct QWidget))))
 ())

(qt-class
 QBoxLayout
 ()
 ((addWidget ((c-pointer (struct QWidget))) void)))

(qt-class
 (QVBoxLayout QBoxLayout)
 (make-QVBoxLayout (QWidget* (c-pointer (struct QWidget))))
 ())

(qt-proxy-class
 ImageWindow
 QMainWindow
 (New-ImageWindow
   (((QWidget* c-pointer) parent #f))
   (lambda (self parent)
     (let* ((UIFileLoc (make-QString "imagewindow.ui"))
            (UIFile (make-QFile UIFileLoc))
            (UILoader (make-QUiLoader self))
            (UIWidget (call load UILoader UIFile self)))
       (call setCentralWidget self UIWidget)
       (call close UIFile)
       (call delete UIFile)
       (call delete UIFileLoc))
     (print "Constructing!")))
 (parent)
 (lambda (self)
   (print "Destructing!"))
 ((SelectImage ((bool bool)) void
               (lambda (self Selected?)
                 (print Selected?)))))

(qt-app:exec-window!
 MyImageWindow
 (New-ImageWindow #f)
 (lambda (Window)
   (call show Window)))