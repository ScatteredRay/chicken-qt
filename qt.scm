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
              cadr
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
    (let ((class-name (cadr e))
          (parent-name '())
          (constructor (caaddr e))
          (constructor-params (cdaddr e)))

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

         (define ,constructor
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
                                   (->string
                                    (if (> i 0)
                                        (string-append
                                         ", ("
                                         (->string (car param))
                                         ")"
                                         (->string (make-name i)))
                                        (make-name i))))
                                 constructor-params 0)
                              "));")))
                         ,@(param-list
                            (lambda (param i)
                              (if (eq? param 'self)
                                  (r 'self)
                                  (make-name i)))
                            constructor-params 0)))))
               ,(r 'self))))
         (qt-foreign-define
          ,class-name
          (delete (c-pointer self))
          void
          ,(string-append
            "delete (("
            (->string class-name)
            "*)self);"))))))

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
               constructor-params))))))))

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
            (qt-class:get-ptr ,window-var))
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
             ;;"w.show();"
             "a.exec();"
             "finalize_window(W);")))))))

(foreign-declare "#include <QtGui/QApplication>")
(foreign-declare "#include <QtGui/QMainWindow>")

(qt-proxy-class
 ImageWindow
 QMainWindow
 (New-ImageWindow
   (((QWidget* c-pointer) parent #f))
   (lambda (self parent)
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
   ((foreign-lambda*
     void
     ((c-pointer Window))
     "((QWidget*)Window)->show();")
    Window)))