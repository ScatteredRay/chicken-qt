(declare (emit-external-prototypes-first))
(require-extension srfi-1)

(define class-name 'ImageWindow)
(define parent-class 'QMainWindow)
(define constructor-params '((QWidget* parent 0)))
(define parent-params '(parent))

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
   (qt-class:get-message-table (qt-class:get-type class))
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
  (lambda (r i)
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
        ,class
        ',method
        (lambda (,(car self)
            ,@(map
               car
               params))
          ((foreign-safe-lambda*
            ,return
            (,(cadr self)
             ,@(map
                cadr
                params))
            ,body)
           (qt-class:get-ptr ,self)
           ,@(map
              car
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
               (make-name r i)))
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
                  (->string (make-name r i))))
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
                         (make-name r i)
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
                               (make-name r i)))
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
                                         (->string (make-name r i)))
                                        (make-name r i))))
                                 constructor-params 0)
                              "));")))
                         ,@(param-list
                            (lambda (param i)
                              (if (eq? param 'self)
                                  (r 'self)
                                  (make-name r i)))
                            constructor-params 0)))))
               ,(r 'self))))))))

(define-syntax qt-proxy-class
  (lambda (e r c)
    (let
        ((class-name (cadr e))
         (parent-class (caddr e))
         (constructor (cadddr e))
         (constructor-params (car (cddddr e)))
         (parent-params (cadr (cddddr e))))
      `(begin
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
              "}"

              "~" (->string class-name) "()"
              "{"
              "CHICKEN_delete_gc_root(proxy_root);"
              "}"

              "};"))))

         (qt-class
          (,class-name ,parent-class)
          (,constructor
           self
           ,@(map
              (lambda (param)
                (car param))
              constructor-params)))))))

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
 New-ImageWindow
 (((QWidget* c-pointer) parent #f))
 (parent))

(qt-app:exec-window!
 MyImageWindow
 (New-ImageWindow #f)
 (lambda (Window)
   ((foreign-lambda*
     void
     ((c-pointer Window))
     "((QWidget*)Window)->show();")
    Window)))