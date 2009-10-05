(declare (emit-external-prototypes-first))

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

(foreign-declare "#include <gcc-plugin.h>")
(foreign-declare "#include <system.h>")
(foreign-declare "#include <coretypes.h>")
(foreign-declare "#include <tree.h>")
(foreign-declare "#include <tree-pass.h>")
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
  ENUMERAL_TYPE)

(define tree-purpose
  (foreign-lambda* c-pointer ((c-pointer arg))
                   "C_return(TREE_PURPOSE((tree)arg));"))

(define identifier-pointer
  (foreign-lambda* c-string ((c-pointer arg))
                   "C_return(IDENTIFIER_POINTER((tree)arg));"))

(define decl-name
  (foreign-lambda* c-pointer ((c-pointer arg))
                   "C_return(DECL_NAME((tree)arg));"))

(define type-name
  (foreign-lambda* c-pointer ((c-pointer arg))
                   "C_return(TYPE_NAME((tree)arg));"))

(define tree-code
  (foreign-lambda* int ((c-pointer arg))
                   "C_return(TREE_CODE((tree)arg));"))

(define tree-code-name
  (foreign-lambda* c-string ((int arg))
                   "C_return(tree_code_name[arg]);"))

(define decl-as-string
  (foreign-lambda* c-string ((c-pointer decl))
                   "tree aname = DECL_P((tree)decl) ? DECL_NAME((tree)decl) : (tree)decl;
					C_return(aname ? IDENTIFIER_POINTER(aname) : \"\");"))

(define-external (handle_struct (c-pointer gcc_data) (c-pointer user_data)) void
  (print (tree-code-name (tree-code gcc_data)))
  (if (eq? (tree-code gcc_data) RECORD_TYPE)
      (print (identifier-pointer (type-name gcc_data)))))

(return-to-host)
