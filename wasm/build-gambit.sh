#!/bin/sh

if test "$1" = ""; then
    printf "usage: $0 <gambit_dir>\n"
    exit 1
fi

# make directory absolute
GAMBIT_DIR="$(cd $1; pwd)"

OUT_DIR="out/gambit"
INCLUDE_DIR="include"

GAMBIT_H="$INCLUDE_DIR/gambit.h"
CONFIG_H="$INCLUDE_DIR/config.h"
STAMP_H="$INCLUDE_DIR/stamp.h"

RTS_MODULES=" \
  $GAMBIT_DIR/lib/main.c \
  $GAMBIT_DIR/lib/setup.c \
  $GAMBIT_DIR/lib/mem.c \
  $GAMBIT_DIR/lib/os_setup.c \
  $GAMBIT_DIR/lib/os_base.c \
  $GAMBIT_DIR/lib/os_time.c \
  $GAMBIT_DIR/lib/os_shell.c \
  $GAMBIT_DIR/lib/os_files.c \
  $GAMBIT_DIR/lib/os_dyn.c \
  $GAMBIT_DIR/lib/os_tty.c \
  $GAMBIT_DIR/lib/os_io.c \
  $GAMBIT_DIR/lib/os_thread.c \
  $GAMBIT_DIR/lib/c_intf.c \
  $GAMBIT_DIR/lib/actlog.c \
"

LIB_MODULES=" \
  $GAMBIT_DIR/lib/_kernel.c \
  $GAMBIT_DIR/lib/_system.c \
  $GAMBIT_DIR/lib/_num.c \
  $GAMBIT_DIR/lib/_std.c \
  $GAMBIT_DIR/lib/_eval.c \
  $GAMBIT_DIR/lib/_io.c \
  $GAMBIT_DIR/lib/_module.c \
  $GAMBIT_DIR/lib/_nonstd.c \
  $GAMBIT_DIR/lib/_thread.c \
  $GAMBIT_DIR/lib/_repl.c \
  $GAMBIT_DIR/lib/_gambit.c \
"

APP_MODULES=" \
  src/_emscripten.c \
  src/_app.c \
"

RTS_LIB_MODULES="$RTS_MODULES $LIB_MODULES"


FLAGS_COMMON="-Wno-unused-value"
FLAGS_OPT="-O2"
FLAGS_OPT_RTS="-O3"

DEFS_LIB=""
DEFS_SH= #"-D___SINGLE_HOST"

COMP_GEN="emcc -I$INCLUDE_DIR $FLAGS_COMMON"
COMP_LIB_MH="$COMP_GEN $FLAGS_OPT $DEFS_LIB -D___LIBRARY"
COMP_LIB_PR_MH="$COMP_GEN $FLAGS_OPT $DEFS_LIB -D___LIBRARY -D___PRIMAL"
COMP_LIB="$COMP_GEN $FLAGS_OPT $DEFS_LIB $DEFS_SH -D___LIBRARY"
COMP_LIB_PR="$COMP_GEN $FLAGS_OPT $DEFS_LIB $DEFS_SH -D___LIBRARY -D___PRIMAL"
COMP_LIB_PR_RTS="$COMP_GEN $FLAGS_OPT_RTS $DEFS_LIB $DEFS_SH -D___LIBRARY -D___PRIMAL"
COMP_APP="$COMP_GEN $FLAGS_OPT $DEFS_SH"

compile_module_c()
{
    file_c="$1"
    file_bc="${file_c%.c}.bc"
    base_c=$(basename $file_c)
    base_bc=$(basename $file_bc)
    out_bc="$OUT_DIR/$base_bc"

    printf ">>> %-15s -> %-15s" "$base_c" "$base_bc"
    if test "$file_c" -nt "$out_bc" -o \
            "$GAMBIT_H" -nt "$out_bc" -o \
            "$CONFIG_H" -nt "$out_bc"; then
        printf "\n"

        case "$base_c" in

            _*) printf "%s " $COMP_LIB_PR -c -o "$out_bc" "$file_c"
                printf "\n"
                $COMP_LIB_PR -c -o "$out_bc" "$file_c" &
                ;;

             *) printf "%s " $COMP_LIB_PR_RTS -c -o "$out_bc" "$file_c"
                printf "\n"
                $COMP_LIB_PR_RTS -c -o "$out_bc" "$file_c" &
                ;;

        esac

    else
        printf "        [using previously compiled file]\n"
    fi

    case "$base_c" in
        _*) TO_LINK_FILES="$TO_LINK_FILES $file_c"
            ;;
    esac

    BC_FILES="$BC_FILES $out_bc"
}

compile_module_scm()
{
    file_scm="$1"
    file_c="${file_scm%.scm}.c"
    base_scm=$(basename $file_scm)
    base_c=$(basename $file_c)
    out_c="$OUT_DIR/$base_c"

    printf ">>> %-15s -> %-15s" "$base_scm" "$base_c"
    if test "$file_scm" -nt "$out_c"; then
        printf "\n"

        printf "%s " gsc -c -o "$out_c" -prelude "(##include\"$GAMBIT_DIR/lib/header.scm\")" "$file_scm"
        printf "\n"

        gsc -c -o "$out_c" -prelude "(##include\"$GAMBIT_DIR/lib/header.scm\")" "$file_scm"
    else
        printf "        [using previously compiled file]\n"
    fi

    compile_module_c "$out_c"
}

compile_rts_lib_modules()
{
    for module in ${RTS_LIB_MODULES}; do
        case "$module" in
              *.c) compile_module_c "$module"
                   ;;
            *.scm) compile_module_scm "$module"
                   ;;
        esac
    done
    wait
}

create_header_files()
{
    if test ! -e "$INCLUDE_DIR"; then
        mkdir "$INCLUDE_DIR"
    fi

    GAMBIT_INCLUDE_DIR="$GAMBIT_DIR/include"

    printf ">>> %-15s -> %-15s" "gambit.h.in" "gambit.h"
    if test "prefix-gambit.h" -nt "$GAMBIT_H" -o \
            "$GAMBIT_INCLUDE_DIR/gambit.h.in" -nt "$GAMBIT_H"; then
        printf "\n"
        cat "prefix-gambit.h" "$GAMBIT_INCLUDE_DIR/gambit.h.in" > "$GAMBIT_H"
    else
        printf "        [using previously generated file]\n"
    fi

    printf ">>> %-15s -> %-15s" "config.h.in" "config.h"
    if test "$GAMBIT_INCLUDE_DIR/config.h.in" -nt "$CONFIG_H"; then
        printf "\n"
        cat "$GAMBIT_INCLUDE_DIR/config.h.in" > "$CONFIG_H"
    else
        printf "        [using previously generated file]\n"
    fi

    printf ">>> %-15s -> %-15s" "stamp.h" "stamp.h"
    if test "$GAMBIT_INCLUDE_DIR/stamp.h" -nt "$STAMP_H"; then
        printf "\n"
        cat "$GAMBIT_INCLUDE_DIR/stamp.h" > "$STAMP_H"
    else
        printf "        [using previously generated file]\n"
    fi
}

link_library()
{
    echo $COMP_LIB_PR ${BC_FILES} -o ${OUT_DIR}/../libgambit.bc
    $COMP_LIB_PR ${BC_FILES} -o ${OUT_DIR}/../libgambit.bc
}

compile_app_modules()
{
    for module in ${APP_MODULES}; do
        file_scm="${module%.c}.scm"
        compile_module_scm "$file_scm"
    done
}

create_link_file()
{
    LAST=""
    for file in ${TO_LINK_FILES}; do
        LAST="$file"
    done

    file_link_c="${LAST%.c}_.c"
    base_link_c=$(basename $file_link_c)
    out_link_c="$OUT_DIR/$base_link_c"
    app_name=$(basename ${LAST%.c}_)

    printf ">>> %-15s -> %-15s\n" "" "$base_link_c"

    printf "%s " gsc -link -flat -o "$out_link_c" -preload ${TO_LINK_FILES}
    printf "\n"

    gsc -link -flat -o "$out_link_c" -preload ${TO_LINK_FILES}

    compile_module_c "$out_link_c"
}

create_filesystem()
{
    if test ! -e "usr"; then

        printf ">>> creating filesystem\n"

        FILES_TAR="`pwd`/files.tar"

        rm -f "$FILES_TAR"

        (cd "$GAMBIT_DIR/lib" && \
         tar cf "$FILES_TAR" *#.scm _*syntax*.scm && \
         find . -name '*.scm' -mindepth 2 -exec tar uf "$FILES_TAR" "{}" ";" && \
         find . -name '*.sld' -mindepth 2 -exec tar uf "$FILES_TAR" "{}" ";")

        mkdir usr
        mkdir usr/local
        mkdir usr/local/Gambit
        mkdir usr/local/Gambit/lib

        (cd usr/local/Gambit/lib && tar xf "$FILES_TAR")

        rm "$FILES_TAR"

    fi
}

link_app()
{
    printf ">>> %-15s -> %-15s\n" "" "$app_name.html"

    printf "%s " emcc $BC_FILES -o "$app_name.html"
    printf "\n"

    emcc $BC_FILES \
         -s EXIT_RUNTIME=1 \
         -s ALLOW_MEMORY_GROWTH=1 \
         -s SAFE_HEAP=0 \
         -s ASM_JS=1 \
         -s WARN_ON_UNDEFINED_SYMBOLS=1 \
         -s EXPORTED_FUNCTIONS="['_setup', '_cleanup', '_idle', '_user_interrupt', '_heartbeat_interrupt']" \
         --no-heap-copy \
         --embed-file usr \
         --embed-file home \
         -o "$app_name.html"

    # Add driver.js in front of generated .js file

    cat driver.js "$app_name.js" > driver-and-app.js
    mv driver-and-app.js "$app_name.js"
}

build()
{
    if test ! -e "$OUT_DIR"; then
        mkdir "$OUT_DIR"
    fi

    BC_FILES=""
    TO_LINK_FILES=""

    create_header_files

    compile_rts_lib_modules

    link_library

    # compile_app_modules

    # create_link_file

    # create_filesystem

    # link_app

    printf ">>> Done!\n"
}

build
