SUBDIRS := #ffi/system

all clean: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

init.o1: init.scm
	@echo "Compiling init module..."
	@rm -f init.o1
	@gsc -cc-options "-I../../external/angle/include" -ld-options "`pkg-config --libs sdl2` -L. -lGLESv2 -lEGL" init.scm && install_name_tool -change @rpath/libEGL.dylib @loader_path/libEGL.dylib init.o1 && install_name_tool -change @rpath/libGLESv2.dylib @loader_path/libGLESv2.dylib init.o1

clean:
	@echo "Cleaning compiled modules..."
	@rm -f init.o1

.PHONY: all clean $(SUBDIRS)

DEBUG_MODULES = -e "(##debug-modules?-set! #t)"
SCHEME_PRELUDE='(begin (define-cond-expand-feature gles2) (load "init.o1"))'

run: init.o1
	@gsi -:dar,search=./,search=../ -e $(SCHEME_PRELUDE) main
