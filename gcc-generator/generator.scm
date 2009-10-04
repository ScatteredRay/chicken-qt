(declare (emit-external-prototypes-first))

(foreign-declare "#include <gcc-plugin.h>")
(foreign-declare "int plugin_is_GPL_compatible;")

(define-external (handle_struct (c-pointer gcc_data) (c-pointer user_data)) void
  (print "HELLO"))

(foreign-declare
"int plugin_init (struct plugin_name_args* plugin_info,
                 struct plugin_gcc_version* version)
{
 const char *plugin_name = plugin_info->base_name;
 register_callback (plugin_name, PLUGIN_FINISH_TYPE, handle_struct, NULL);
 return 0;
}" )