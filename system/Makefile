BGFX_DIR = /Users/alvatar/Dropbox/projects/bgfx

GL_INCLUDE = /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks/OpenGL.framework/Headers

BGFX_HEADERS =  -I$(BGFX_DIR)/bgfx/include \
		-I$(BGFX_DIR)/bx/include \
		-I$(BGFX_DIR)/bimg/include \
		-I/usr/local/Cellar/sdl2/2.0.10/include \
		-I$(GL_INCLUDE)

BGFX_LIB = -L../lib -lbgfx-shared-libDebug
BGFX_LINK = libbgfx-shared-libDebug.dylib

SDL2_LIB = -L/usr/local/Cellar/sdl2/2.0.10/lib -lSDL2

LINKER_FLAGS = $(BGFX_LIB) $(SDL2_LIB)
EXE_FILE = main

cintf: cintf_bridge.cpp cintf.scm
	clang $(CC_FLAGS) -c -o cintf_bridge.o -g cintf_bridge.cpp $(COMPILER_FLAGS) $(BGFX_HEADERS) #$(LINKER_FLAGS)
	gsc -link -flat -o cintf_core.o1.c cintf
	gsc -cc-options "-D___DYNAMIC" -obj cintf.c cintf_core.o1.c
	clang -bundle cintf_bridge.o cintf.o cintf_core.o1.o $(LINKER_FLAGS) -o cintf_core.o1
	install_name_tool -change ../../osx64_clang/bin/$(BGFX_LINK) @loader_path/../lib/$(BGFX_LINK) cintf_core.o1
	rm cintf.c cintf.o cintf_bridge.o cintf_core.o1.c cintf_core.o1.o

all: cintf
