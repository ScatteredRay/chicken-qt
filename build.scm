(load "buildvar.scm")

(defvar CSC ~/bin/chicken4/bin/csc)

(defvar DEFINES -DQT_NO_DEBUG -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -DAUTODETECT_COCOA)
(defvar CFLAGS -no-cpp-precomp -fno-strict-aliasing -fno-common -DHAVE_CHICKEN_CONFIG_H -arch x86_64 -m64 -fomit-frame-pointer -pipe -Os -W ,@DEFINES -g)
(defvar CXXFLAGS ,@CFLAGS)
(defvar INCPATH -I/Users/Indy/bin/chicken4/include -I/usr/local/Qt4.5/mkspecs/macx-g++ -I. -I/usr/local/Trolltech/Qt-4.5.3/include -I/usr/include -Irelease -I. -F/Library/Framework -F/usr/local/Trolltech/Qt-4.5.3/libs)
(defvar LFLAGS -headerpad_max_install_names -g -m64)
(defvar LIBS -F/Library/Frameworks -F/usr/local/Trolltech/Qt-4.5.3/lib -L/usr/local/Trolltech/Qt-4.5.3/lib -L/Library/Frameworks -framework QtOpenGL -framework QtGui -framework AppKit -framework QtCore -lz -lm -framework ApplicationServices -L/Users/Indy/bin/chicken4/lib -lchicken)

(defvar OUTFILE qtchicken)

(command CSC '-c++ '-t 'qt.scm)

(build-cxx '(qt.cpp))

(command LINK '-o OUTFILE LFLAGS LIBS OBJECT_FILES)

(exit)