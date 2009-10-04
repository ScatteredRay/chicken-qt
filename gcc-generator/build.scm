(load "../buildvar.scm")

(defvar CSC ~/local/chicken4/bin/csc)

(defvar DEFINES)
(defvar CFLAGS -no-cpp-precomp -fno-strict-aliasing -fno-common -DHAVE_CHICKEN_CONFIG_H -arch x86_64 -m64 -fomit-frame-pointer -pipe -Os -W ,@DEFINES -g)
(defvar CXXFLAGS ,@CFLAGS)
(defvar INCPATH -I/Users/Indy/local/chicken4/include -I/Users/Indy/local/gcc45/include -I/Users/Indy/dev/gcc/gcc -I/Users/Indy/dev/gcc/include -I/Users/Indy/dev/gcc/host-i386-apple-darwin9.8.0/gcc -I/usr/include -I.)
(defvar LFLAGS -headerpad_max_install_names -g -m64 -dylib -bundle -flat_namespace -undefined suppress -Wl -dynamic_lookup)
(defvar LIBS -F/Library/Frameworks -lz -lm -L/Users/Indy/local/chicken4/lib -L/Users/Indy/local/gcc45/lib -lchicken -lstdc++)

(defvar OUTFILE libchickendump.dylib)

(command CSC '-t 'generator.scm)

(build-c '(generator.c))

(command LINK '-o OUTFILE LFLAGS LIBS OBJECT_FILES)
(command 'install_name_tool '-change 'libchicken.dylib '/Users/Indy/local/chicken4/lib/libchicken.dylib OUTFILE)

(exit)