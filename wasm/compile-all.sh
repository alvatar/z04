#!/usr/bin/env bash

#########################################################
# General config
#########################################################

if [ -z $1 ]; then
    echo "Please provide Gambit's absolute path as argument"
    exit
fi

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
# Gambit Runtime
#########################################################

GAMBIT_DIR=$1
GAMBIT_LIB_DIR="$GAMBIT_DIR/lib"

echo "#define ___VOIDSTAR_WIDTH 32
#define ___MAX_CHR 0x10ffff
#define ___SINGLE_VM
#define ___SINGLE_THREADED_VMS
#define ___USE_NO_THREAD_SYSTEM
#define ___NO_THREAD_LOCAL_STORAGE_CLASS
#define ___DONT_HAVE_CONDITION_VARIABLE
#define ___NO_ACTIVITY_LOG
#define ___BOOL int
#define ___USE_NO_SIGSET_T

#define HAVE_STDLIB_H
#define HAVE_ERRNO_H
#define HAVE_STRING_H
#define HAVE_EMSCRIPTEN_H
#define HAVE_EMSCRIPTEN_GET_NOW

// Line buffering for print functions
#define ___DEFAULT_RUNTIME_OPTIONS {'t', 'u', ',', '-', 'u', '\0'}

#define ___GAMBIT_DIR \"/usr/local/Gambit\"
#define ___GAMBITDIR \"/usr/local/Gambit\"
" > gambit.h
cat $GAMBIT_DIR/include/gambit.h.in >> gambit.h

FLAGS_COMMON=
FLAGS_OPT="-O2" #"-Oityb1"
FLAGS_OPT_RTS="-O2" #"-Oityb2"

DEFS_LIB=
DEFS_SH= #"-D___SINGLE_HOST"

COMP_GEN="emcc -I. -I$GAMBIT_DIR/include $FLAGS_COMMON"
COMP_LIB_MH="$COMP_GEN $FLAGS_OPT $DEFS_LIB -D___LIBRARY"
COMP_LIB_PR_MH="$COMP_GEN $FLAGS_OPT $DEFS_LIB -D___LIBRARY -D___PRIMAL"
COMP_LIB="$COMP_GEN $FLAGS_OPT $DEFS_LIB $DEFS_SH -D___LIBRARY"
COMP_LIB_PR="$COMP_GEN $FLAGS_OPT $DEFS_LIB $DEFS_SH -D___LIBRARY -D___PRIMAL"
COMP_LIB_PR_RTS="$COMP_GEN $FLAGS_OPT_RTS $DEFS_LIB $DEFS_SH -D___LIBRARY -D___PRIMAL"
COMP_APP="$COMP_GEN $FLAGS_OPT $DEFS_SH"

GAMBIT_OUTDIR=out/gambit

compile_gambit_runtime() {
    echo "Compiling Gambit..."

    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/main.bc $GAMBIT_LIB_DIR/main.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/setup.bc $GAMBIT_LIB_DIR/setup.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/mem.bc $GAMBIT_LIB_DIR/mem.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_setup.bc $GAMBIT_LIB_DIR/os_setup.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_base.bc $GAMBIT_LIB_DIR/os_base.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_time.bc $GAMBIT_LIB_DIR/os_time.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_shell.bc $GAMBIT_LIB_DIR/os_shell.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_files.bc $GAMBIT_LIB_DIR/os_files.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_dyn.bc $GAMBIT_LIB_DIR/os_dyn.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_tty.bc $GAMBIT_LIB_DIR/os_tty.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_io.bc $GAMBIT_LIB_DIR/os_io.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/os_thread.bc $GAMBIT_LIB_DIR/os_thread.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/c_intf.bc $GAMBIT_LIB_DIR/c_intf.c
    $COMP_LIB_PR_RTS -c -o $GAMBIT_OUTDIR/actlog.bc $GAMBIT_LIB_DIR/actlog.c

    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_kernel.bc $GAMBIT_LIB_DIR/_kernel.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_system.bc $GAMBIT_LIB_DIR/_system.c
    $COMP_LIB_PR_MH -c -o $GAMBIT_OUTDIR/_num.bc $GAMBIT_LIB_DIR/_num.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_std.bc $GAMBIT_LIB_DIR/_std.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_eval.bc $GAMBIT_LIB_DIR/_eval.c
    $COMP_LIB_PR_MH -c -o $GAMBIT_OUTDIR/_io.bc $GAMBIT_LIB_DIR/_io.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_module.bc $GAMBIT_LIB_DIR/_module.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_nonstd.bc $GAMBIT_LIB_DIR/_nonstd.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_thread.bc $GAMBIT_LIB_DIR/_thread.c
    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_repl.bc $GAMBIT_LIB_DIR/_repl.c

    $COMP_LIB_PR -c -o $GAMBIT_OUTDIR/_gambit.bc $GAMBIT_LIB_DIR/_gambit.c

    $COMP_LIB_PR $GAMBIT_OUTDIR/main.bc \
                 $GAMBIT_OUTDIR/setup.bc \
                 $GAMBIT_OUTDIR/mem.bc \
                 $GAMBIT_OUTDIR/os_setup.bc \
                 $GAMBIT_OUTDIR/os_base.bc \
                 $GAMBIT_OUTDIR/os_time.bc \
                 $GAMBIT_OUTDIR/os_shell.bc \
                 $GAMBIT_OUTDIR/os_files.bc \
                 $GAMBIT_OUTDIR/os_dyn.bc \
                 $GAMBIT_OUTDIR/os_tty.bc \
                 $GAMBIT_OUTDIR/os_io.bc \
                 $GAMBIT_OUTDIR/os_thread.bc \
                 $GAMBIT_OUTDIR/c_intf.bc \
                 $GAMBIT_OUTDIR/actlog.bc \
                 $GAMBIT_OUTDIR/_kernel.bc \
                 $GAMBIT_OUTDIR/_system.bc \
                 $GAMBIT_OUTDIR/_num.bc \
                 $GAMBIT_OUTDIR/_std.bc \
                 $GAMBIT_OUTDIR/_eval.bc \
                 $GAMBIT_OUTDIR/_io.bc \
                 $GAMBIT_OUTDIR/_module.bc \
                 $GAMBIT_OUTDIR/_nonstd.bc \
                 $GAMBIT_OUTDIR/_thread.bc \
                 $GAMBIT_OUTDIR/_repl.bc \
                 $GAMBIT_OUTDIR/_gambit.bc \
                 -o $OUTDIR/libgambit.bc

    return 1
}

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
SCHEME_C_FILES="${SCHEME_C_FILES} out/std.c"
SCHEME_BC_FILES="${SCHEME_BC_FILES} out/std.bc"

compile_std_libs() {
    for lib in $STD_LIBS; do
        MODULE=$(dirname $lib)
        echo "Compiling Gambit library" ${MODULE} "..."
        mkdir -p $STD_OUTDIR/$MODULE
        gsc -:search=./,search=.. -c -o $STD_OUTDIR/$MODULE.c -module-ref $MODULE $GAMBIT_LIB_DIR/$lib.sld
        emcc -I$WORKDIR -c $STD_OUTDIR/$MODULE.c $C_FLAGS -o $STD_OUTDIR/$MODULE.bc
    done

    gsc -:search=./,search=.. -c -o out/std.c -module-ref std ../std.scm
    emcc -I$WORKDIR -c out/std.c $C_FLAGS -o out/std.bc
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
        gsc -c -module-ref $(dirname ${lib}) -e '(define-cond-expand-feature emscripten)' $FILE
        mkdir -p $LIBS_OUTDIR/$LOCAL_DIR
        cp ${FILE//.sld}.c $LIBS_OUTDIR/$LOCAL_DIR
        emcc $EMCC_LIB_FLAGS -I$WORKDIR -c $TARGET_FILE.c $C_FLAGS -o $TARGET_FILE.bc
        cd $WORKDIR
    done
}

#########################################################
# Main Project
#########################################################

SCHEME_PROJ_LIBS="
core/core.sld
render/render.sld
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
        gsc -:search=./,search=.. -c -o $TARGET_FILE.c -e '(define-cond-expand-feature emscripten)' $lib
        emcc -I$WORKDIR -c $TARGET_FILE.c $C_FLAGS -o $TARGET_FILE.bc
        cd $WORKDIR
    done
}

compile_main() {
    echo "Compiling Scheme main..."
    cd $SCHEME_PROJ_DIR
    gsc -:search=./,search=.. -c -o $PROJ_OUTDIR/main.c -e '(define-cond-expand-feature emscripten)' main.scm
    cd $WORKDIR
    emcc -I$SCHEME_PROJ_DIR -I$WORKDIR -c -o $PROJ_OUTDIR/main.bc $PROJ_OUTDIR/main.c
}

#########################################################
# Compile all and link
#########################################################

if [ ! -f $OUTDIR/libgambit.bc ]; then
    mkdir -p $GAMBIT_OUTDIR
    compile_gambit_runtime
fi

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

compile_main

echo "Compiling link file..."
gsc -warnings -link -o $OUTDIR/app_.c -nopreload $SCHEME_C_FILES $PROJ_OUTDIR/main.c
emcc -I$WORKDIR $OUTDIR/app_.c -c -o $OUTDIR/app_.bc

echo "Linking..."
emcc $EMCC_LIB_FLAGS -s WASM=1 \
     --preload-file assets \
     $OUTDIR/libgambit.bc \
     $SCHEME_BC_FILES \
     $PROJ_OUTDIR/main.bc \
     $OUTDIR/app_.bc \
     -o app.html
