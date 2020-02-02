#!/bin/sh

if test "$1" = ""; then
    printf "usage: $0 <gambit_dir>\n"
    exit 1
fi

#########################################################
# General config
#########################################################

GAMBIT_DIR=$1
GAMBIT_LIB_DIR="$GAMBIT_DIR/lib"
WORKDIR=`pwd`
OUTDIR="out"
SCHEME_PROJ_DIR=".."
PROJ_OUTDIR=$WORKDIR/$OUTDIR/scheme
SCHEME_LIBS_DIR="../.."
LIBS_OUTDIR=$WORKDIR/$OUTDIR/scheme-libs
STD_OUTDIR=$WORKDIR/$OUTDIR/scheme-stdlibs

C_FLAGS="-Wno-incompatible-pointer-types-discards-qualifiers -Wno-incompatible-pointer-types"

SCHEME_C_FILES=""
SCHEME_BC_FILES=""


#########################################################
# Std libraries
#########################################################

STD_LIBS="
srfi/28/28
srfi/69/69
"

for lib in $STD_LIBS; do
    MODULE=$(dirname $lib)
    SCHEME_C_FILES="${SCHEME_C_FILES} $STD_OUTDIR/$MODULE.c"
    SCHEME_BC_FILES="${SCHEME_BC_FILES} $STD_OUTDIR/$MODULE.bc"
done
SCHEME_C_FILES="${SCHEME_C_FILES} out/std.c out/emscripten.c"
SCHEME_BC_FILES="${SCHEME_BC_FILES} out/std.bc out/emscripten.bc"

compile_std_libs() {
    for lib in $STD_LIBS; do
        MODULE=$(dirname $lib)
        echo "Compiling Gambit library" ${MODULE} "..."
        mkdir -p $STD_OUTDIR/$MODULE
        $(gsc -:search=./,search=.. -c -o $STD_OUTDIR/$MODULE.c -module-ref $MODULE $GAMBIT_LIB_DIR/$lib.sld &&
          emcc -I$WORKDIR -c $STD_OUTDIR/$MODULE.c $C_FLAGS -o $STD_OUTDIR/$MODULE.bc) &
    done

    echo "Compiling Gambit std library..."
    $(gsc -:search=./,search=.. -c -o out/std.c -module-ref std ../std.scm &&
      emcc -I$WORKDIR -c out/std.c $C_FLAGS -o out/std.bc) &

    echo "Compiling Gambit emscripten library..."
    $(gsc -:search=./,search=.. -c -o out/emscripten.c -prelude "(##include\"$GAMBIT_DIR/lib/header.scm\")" ../emscripten.scm &&
      emcc -I$WORKDIR -c out/emscripten.c $C_FLAGS -o out/emscripten.bc) &
}

#########################################################
# Scheme Libraries
#########################################################

EMCC_LIB_FLAGS="-s USE_SDL=2 -s USE_SDL_TTF=2"

SCHEME_LIBS="
base/base.sld
base/alist/alist.sld
base/functional/functional.sld
ffi-utils/ffi-utils.sld
math/matrix/matrix.sld
base/memoization/memoization.sld
math/vector2/vector2.sld
sdl2/sdl2.sld
sdl2/ttf/ttf.sld
gles2/gles2.sld
"
#base/functional/combinator/combinator.sld

for lib in $SCHEME_LIBS; do
    SOURCE_DIR=`dirname $SCHEME_LIBS_DIR/$lib`
    FILE=`basename $SCHEME_LIBS_DIR/$lib`
    LOCAL_DIR=`dirname $lib`
    TARGET_FILE=$(sed -E 's/(.sld|.scm)//g' <<< $LIBS_OUTDIR/$LOCAL_DIR/$FILE)
    SCHEME_C_FILES="${SCHEME_C_FILES} $TARGET_FILE.c"
    SCHEME_BC_FILES="${SCHEME_BC_FILES} $TARGET_FILE.bc"
done

compile_scheme_libs() {
    for lib in $SCHEME_LIBS; do
        echo "Compiling Scheme library" $lib "..."
        SOURCE_DIR=`dirname $SCHEME_LIBS_DIR/$lib`
        FILE=`basename $SCHEME_LIBS_DIR/$lib`
        LOCAL_DIR=`dirname $lib`
        TARGET_FILE=$(sed -E 's/(.sld|.scm)//g' <<< $LIBS_OUTDIR/$LOCAL_DIR/$FILE)
        cd $SOURCE_DIR
        $(gsc -c -module-ref $(dirname ${lib}) -e '(define-cond-expand-feature emscripten)' $FILE &&
          mkdir -p $LIBS_OUTDIR/$LOCAL_DIR &&
          cp ${FILE//.sld}.c $LIBS_OUTDIR/$LOCAL_DIR &&
          emcc $EMCC_LIB_FLAGS -I$WORKDIR -c $TARGET_FILE.c $C_FLAGS -o $TARGET_FILE.bc) &
        cd $WORKDIR
    done
}

#########################################################
# Main Project
#########################################################

SCHEME_PROJ_LIBS="
core/core.sld
render/render.sld
json/json.sld
"

for lib in $SCHEME_PROJ_LIBS; do
    LOCAL_DIR=`dirname $lib`
    TARGET_FILE=$(sed -E 's/(.sld|.scm)//g' <<< $PROJ_OUTDIR/$lib)
    SCHEME_C_FILES="${SCHEME_C_FILES} $TARGET_FILE.c"
    SCHEME_BC_FILES="${SCHEME_BC_FILES} $TARGET_FILE.bc"
done

compile_project() {
    for lib in $SCHEME_PROJ_LIBS; do
        echo "Compiling Scheme project file" $lib "..."
        LOCAL_DIR=`dirname $lib`
        TARGET_FILE=$(sed -E 's/(.sld|.scm)//g' <<< $PROJ_OUTDIR/$lib)
        cd $SCHEME_PROJ_DIR
        mkdir -p $PROJ_OUTDIR/$LOCAL_DIR
        #gsc -:search=./,search=.. -c -o $TARGET_FILE.c -e '(import _define-library/debug)' $lib > $TARGET_FILE.expansion.scm
        #gsc -:search=./,search=.. -c -o $TARGET_FILE.c -expansion $lib > $TARGET_FILE.expansion.scm
        $(gsc -:search=./,search=.. -c -o $TARGET_FILE.c -e '(define-cond-expand-feature emscripten)' $lib &&
          emcc -I$WORKDIR -c $TARGET_FILE.c $C_FLAGS -o $TARGET_FILE.bc) &
        cd $WORKDIR
    done
}

compile_main() {
    echo "Compiling Scheme main..."
    cd $SCHEME_PROJ_DIR
    gsc -:search=./,search=.. -c -o $PROJ_OUTDIR/main.c -e '(define-cond-expand-feature emscripten)' main.scm
    cd $WORKDIR
    emcc $EMCC_LIB_FLAGS -I$SCHEME_PROJ_DIR -I$WORKDIR -c -o $PROJ_OUTDIR/main.bc $PROJ_OUTDIR/main.c
}

#########################################################
# Compile all and link
#########################################################

if [ ! -d "$STD_OUTDIR" ]; then
    mkdir -p $STD_OUTDIR
    compile_std_libs
fi

if [ ! -d "$LIBS_OUTDIR" ]; then
    mkdir -p $LIBS_OUTDIR
    compile_scheme_libs
fi

if [ ! -d "$PROJ_OUTDIR" ]; then
    mkdir -p $PROJ_OUTDIR
    compile_project
fi

wait

compile_main

echo "Compiling link file..."
gsc -warnings -link -o $OUTDIR/app_.c -nopreload $SCHEME_C_FILES $PROJ_OUTDIR/main.c
emcc -I$WORKDIR $OUTDIR/app_.c -c -o $OUTDIR/app_.bc

echo "Linking..."
emcc $EMCC_LIB_FLAGS \
     -s EXIT_RUNTIME=1 \
     -s ALLOW_MEMORY_GROWTH=1 \
     -s SAFE_HEAP=0 \
     -s ASM_JS=1 \
     -s WARN_ON_UNDEFINED_SYMBOLS=1 \
     -s WASM=1 \
     -s EXPORTED_FUNCTIONS='["_main", "_js_push_event"]' \
     -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' \
     --no-heap-copy \
     --preload-file assets \
     $OUTDIR/libgambit.bc \
     $SCHEME_BC_FILES \
     $PROJ_OUTDIR/main.bc \
     $OUTDIR/app_.bc \
     -o app.html
# -s EXPORTED_FUNCTIONS="['_setup', '_cleanup', '_idle', '_user_interrupt', '_heartbeat_interrupt', '_main', '_js_push_event']" \

