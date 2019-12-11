SUBDIRS := ffi/system

all clean: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

all:
	@echo "Compiling FFI modules..."
	@gsc . ffi/memory

clean:
	@echo "Cleaning FFI modules..."
	@rm -fR ffi/memory@*

.PHONY: all clean $(SUBDIRS)

DEBUG_MODULES = -e "(##debug-modules?-set! #t)"

run:
	@gsi -:search=./,search=../ main
