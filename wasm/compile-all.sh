#!/bin/sh

if [ -z $1 ]; then
    echo "Please provide Gambit's absolute path as argument"
    exit
fi

GAMBIT_DIR=$1
GAMBIT_LIB_DIR="$GAMBIT_DIR/lib"

echo '#define ___VOIDSTAR_WIDTH 32
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

#define ___GAMBIT_DIR "/usr/local/Gambit"
#define ___GAMBITDIR "/usr/local/Gambit"
' > gambit.h
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

$COMP_LIB_PR_RTS -o out/main.bc $GAMBIT_LIB_DIR/main.c
$COMP_LIB_PR_RTS -o out/setup.bc $GAMBIT_LIB_DIR/setup.c
$COMP_LIB_PR_RTS -o out/mem.bc $GAMBIT_LIB_DIR/mem.c
$COMP_LIB_PR_RTS -o out/os_setup.bc $GAMBIT_LIB_DIR/os_setup.c
$COMP_LIB_PR_RTS -o out/os_base.bc $GAMBIT_LIB_DIR/os_base.c
$COMP_LIB_PR_RTS -o out/os_time.bc $GAMBIT_LIB_DIR/os_time.c
$COMP_LIB_PR_RTS -o out/os_shell.bc $GAMBIT_LIB_DIR/os_shell.c
$COMP_LIB_PR_RTS -o out/os_files.bc $GAMBIT_LIB_DIR/os_files.c
$COMP_LIB_PR_RTS -o out/os_dyn.bc $GAMBIT_LIB_DIR/os_dyn.c
$COMP_LIB_PR_RTS -o out/os_tty.bc $GAMBIT_LIB_DIR/os_tty.c
$COMP_LIB_PR_RTS -o out/os_io.bc $GAMBIT_LIB_DIR/os_io.c
$COMP_LIB_PR_RTS -o out/os_thread.bc $GAMBIT_LIB_DIR/os_thread.c
$COMP_LIB_PR_RTS -o out/c_intf.bc $GAMBIT_LIB_DIR/c_intf.c
$COMP_LIB_PR_RTS -o out/actlog.bc $GAMBIT_LIB_DIR/actlog.c

$COMP_LIB_PR -o out/_kernel.bc $GAMBIT_LIB_DIR/_kernel.c
$COMP_LIB_PR -o out/_system.bc $GAMBIT_LIB_DIR/_system.c
$COMP_LIB_PR_MH -o out/_num.bc $GAMBIT_LIB_DIR/_num.c
$COMP_LIB_PR -o out/_std.bc $GAMBIT_LIB_DIR/_std.c
$COMP_LIB_PR -o out/_eval.bc $GAMBIT_LIB_DIR/_eval.c
$COMP_LIB_PR -o out/_io.bc $GAMBIT_LIB_DIR/_io.c
$COMP_LIB_PR -o out/_module.bc $GAMBIT_LIB_DIR/_module.c
$COMP_LIB_PR -o out/_nonstd.bc $GAMBIT_LIB_DIR/_nonstd.c
$COMP_LIB_PR -o out/_thread.bc $GAMBIT_LIB_DIR/_thread.c
$COMP_LIB_PR -o out/_repl.bc $GAMBIT_LIB_DIR/_repl.c

$COMP_LIB_PR -o out/_gambit.bc $GAMBIT_LIB_DIR/_gambit.c

$COMP_LIB_PR out/main.bc \
             out/setup.bc \
             out/mem.bc \
             out/os_setup.bc \
             out/os_base.bc \
             out/os_time.bc \
             out/os_shell.bc \
             out/os_files.bc \
             out/os_dyn.bc \
             out/os_tty.bc \
             out/os_io.bc \
             out/os_thread.bc \
             out/c_intf.bc \
             out/actlog.bc \
             out/_kernel.bc \
             out/_system.bc \
             out/_num.bc \
             out/_std.bc \
             out/_eval.bc \
             out/_io.bc \
             out/_module.bc \
             out/_nonstd.bc \
             out/_thread.bc \
             out/_repl.bc \
             out/_gambit.bc \
             -o out/libgambit.bc
