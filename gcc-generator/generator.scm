(declare (emit-external-prototypes-first))

(import-for-syntax chicken)
(import-for-syntax matchable)

(define-syntax define-foreign-enum
  (lambda (e r c)
    `(,(r 'begin)
      ,@(map
         (match-lambda
          ((name realname)
           `(,(r 'define) ,name (,(r 'foreign-value) ,(symbol->string realname) ,(r 'int))))
          (name `(,(r 'define) ,name (,(r 'foreign-value) ,(symbol->string name) ,(r 'int)))))
         (cdr e)))))

(foreign-declare "#include <config.h>")
(foreign-declare "#include <gcc-plugin.h>")
(foreign-declare "#include <system.h>")
(foreign-declare "#include <coretypes.h>")
(foreign-declare "#include <tree.h>")
(foreign-declare "#include <tree-pass.h>")
(foreign-declare "#define CUMULATIVE_ARGS int") ;; Not defined for some reason.
(foreign-declare "#include <cp/cp-tree.h>")
(foreign-declare "int plugin_is_GPL_compatible;")

(foreign-declare
"int plugin_init (struct plugin_name_args* plugin_info,
                 struct plugin_gcc_version* version)
{
 CHICKEN_initialize(0, 0, 0, C_toplevel);
 CHICKEN_run(NULL);
 const char *plugin_name = plugin_info->base_name;
 register_callback (plugin_name, PLUGIN_FINISH_TYPE, handle_struct, NULL);
 return 0;
}" )

(define-foreign-enum
  RECORD_TYPE
  UNION_TYPE
  ENUMERAL_TYPE
  POINTER_TYPE)

(define void-list-node?
  (foreign-lambda* bool ((c-pointer list_node))
                   "C_return(list_node == void_list_node);"))

(define-foreign-enum
  NULL_TREE) ;; Not actually an enum
(define (gcc-null-list? x) (not x))

(define-for-syntax gcc-rename
  (compose string->symbol string-downcase (cut string-translate <> "_" "-") symbol->string))

(define-syntax define-gcc-tree-func
  (lambda (e r c)
    (match
     e
     (('define-gcc-tree-func CName ret)
      `(define
         ,(gcc-rename CName)
         (foreign-lambda* ,ret ((c-pointer arg))
                          ,(string-append
                            "C_return("
                            (symbol->string CName)
                            "((tree)arg));")))))))

(define-syntax gcc-tree-func-list
  (lambda (e r c)
    `(begin
       ,@(map
          (lambda (tree-func)
            `(define-gcc-tree-func ,@tree-func))
          (cdr e)))))

(gcc-tree-func-list
 (IDENTIFIER_POINTER c-string)
 (DECL_NAME c-pointer)
 (DECL_ASSEMBLER_NAME c-pointer)
 (TYPE_NAME c-pointer)
 (TREE_CODE int)
 (TREE_TYPE c-pointer)
 (TREE_PURPOSE c-pointer)
 (TREE_VALUE c-pointer)
 (TREE_CHAIN c-pointer)
 (TYPE_ARG_TYPES c-pointer)
 (CLASSTYPE_DECLARED_CLASS bool)
 (TYPE_METHODS c-pointer)
 (DECL_RESULT c-pointer)
 (DECL_ARGUMENTS c-pointer)
 (DECL_CONTEXT c-pointer))

(define tree-code-name
  (foreign-lambda* c-string ((int arg))
                   "C_return(tree_code_name[arg]);"))

(define decl-as-string
  (foreign-lambda* c-string ((c-pointer decl))
                   "tree aname = DECL_P((tree)decl) ? DECL_NAME((tree)decl) : (tree)decl;
					C_return(aname ? IDENTIFIER_POINTER(aname) : \"\");"))

(define (gcc-type-name type)
  (if (eq? (tree-code type)
           POINTER_TYPE)
      (string-append (gcc-type-name (tree-type type)) "*")
      (identifier-pointer (decl-name (type-name type)))))

(define-external (handle_struct (c-pointer gcc_data) (c-pointer user_data)) void
  (when (and
         (eq? (tree-code gcc_data) RECORD_TYPE)
         (classtype-declared-class gcc_data))
    (newline)
    (newline)
    (print (identifier-pointer (decl-name (type-name gcc_data))))
    (letrec ((dump-methods
              (lambda (method-lst)
                (if (gcc-null-list? method-lst)
                 '()
                 (let*
                     ((func-type (tree-type method-lst))
                      (result-type (tree-type func-type)))
                   (newline)
                   (print (gcc-type-name result-type))
                   (print (identifier-pointer (decl-name method-lst)))
                   (letrec ((dump-arguments
                             (lambda (arg-list)
                               (if (or
                                    (gcc-null-list? arg-list) ;; If we get this before a void list node then it's vararg
                                    (void-list-node? arg-list)) 
                                   '()
                                   (begin
                                     (print (gcc-type-name (tree-value arg-list)))
                                     (dump-arguments (tree-chain arg-list)))))))
                     (dump-arguments (type-arg-types func-type)))
                   (dump-methods (tree-chain method-lst)))))))
      (dump-methods (type-methods gcc_data)))))

(return-to-host)
