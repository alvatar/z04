SUBDIRS := ffi/system

all clean: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

all:
	@echo "Compiling FFI modules..."
	@gsc -module-ref ffi/memory . ffi/memory

clean:
	@echo "Cleaning FFI modules..."
	@rm -fR ffi/memory@*

.PHONY: all clean $(SUBDIRS)

run:
	@gsi -:search=./,search=../scheme-libs/ main
