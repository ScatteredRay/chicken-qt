(require-extension riaxpander)
(require-extension srfi-1)
(require-extension filepath)
(require-extension files)

(use posix)
 
(define-syntax defvar
  (syntax-rules ()
      ((defvar var params ...)
       (define var `(params ...)))))

(define-syntax appendvar
  (syntax-rules ()
	((appendvar var params ...)
	 (set! var (append! var `(params ...))))))

(define (command . x)
  (system
   (apply
    string-append
    (intersperse
     (map ->string
          (concatenate
           (map (lambda (x)
                  (if (list? x)
                      x
                      (list x)))
                x)))
     " "))))

(define (command-rslt . x)
  (with-input-from-pipe
      (apply
       (string-append
        (intersperse
         (map ->string
              (concatenate
               (map (lambda (x)
                      (if (list? x)
                          x
                          (list x)))
                    x)))
         " ")))
      read-lines))

(define OBJECT_FILES '())

(defvar CC gcc)
(defvar CXX g++)
(defvar LINK g++)
(defvar CSC csc)
(defvar MOC moc)
(defvar CSI csi)
(defvar UIC uic)

(define (build file)
  (command CSI file))

(define (build-moc MOCHEADERS)
  (for-each
   (lambda (file)
	 (let ((outfile
			(string-append
			 "moc_"
			 (pathname-strip-extension (->string file))
			 ".cpp")))
       (command MOC file '-o outfile)
       (append! SOURCES (list outfile))))
   MOCHEADERS))

(define (build-cxx SOURCES)
 (for-each
  (lambda (file)
	(command CXX '-c CXXFLAGS INCPATH file)
	(set!
	 OBJECT_FILES
	 (cons
	  (string-append
	   (pathname-strip-extension (->string file))
	   ".o")
	  OBJECT_FILES)))
  SOURCES))