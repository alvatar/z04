/* File: "prefix-gambit.h" */

/*
 * Copyright (c) 2020 by Marc Feeley, All Rights Reserved.
 */

/*
 * This file must be appended to the head of the gambit.h.in file to
 * create a gambit.h file that is appropriate for emscripten.
 */

/*---------------------------------------------------------------------------*/

#define ___GAMBITDIR "/usr/local/Gambit"

#define ___GAMBITDIR_USERLIB "~/.gambit_userlib"

#define ___DEFAULT_RUNTIME_OPTIONS { '\0' }

#define ___HEARTBEAT_USING_POLL_COUNTDOWN 200000

/*---------------------------------------------------------------------------*/

/* Various settings appropriate for emcc compiler */

#define ___VOIDSTAR_WIDTH 32
#define ___MAX_CHR 0x10ffff
#define ___SINGLE_VM
#define ___SINGLE_THREADED_VMS
#define ___USE_NO_THREAD_SYSTEM
#define ___NO_THREAD_LOCAL_STORAGE_CLASS
#define ___DONT_HAVE_CONDITION_VARIABLE
#define ___NO_ACTIVITY_LOG
#define ___BOOL int
#define ___USE_SIGSET_T

/*---------------------------------------------------------------------------*/

/* Header files available in emscripten environment */

#define HAVE_DIRENT_H
#define HAVE_DLFCN_H
#define HAVE_EMSCRIPTEN
#define HAVE_ERRNO_H
#define HAVE_FCNTL_H
#define HAVE_FLOAT_H
#define HAVE_GRP_H
#define HAVE_NETDB_H
#define HAVE_PTY_H
#define HAVE_PWD_H
#define HAVE_STDIO_H
#define HAVE_STDLIB_H
#define HAVE_STRING_H
#define HAVE_SYSLOG_H
#define HAVE_SYS_IOCTL_H
#define HAVE_SYS_MMAN_H
#define HAVE_SYS_RESOURCE_H
#define HAVE_SYS_SOCKET_H
#define HAVE_SYS_STAT_H
#define HAVE_SYS_SYSCTL_H
#define HAVE_SYS_TIME_H
#define HAVE_SYS_TYPES_H
#define HAVE_SYS_WAIT_H
#define HAVE_TERMIOS_H
#define HAVE_TERMIOS_H
#define HAVE_TIME_H
#define HAVE_UNISTD_H

/*---------------------------------------------------------------------------*/

/* Library functions available in emscripten environment */

#define HAVE_CHDIR
#define HAVE_CLOCK_GETTIME
#define HAVE_DLOPEN
#define HAVE_ENVIRON
#define HAVE_ERRNO
#define HAVE_EXECVP
#define HAVE_FCNTL
#define HAVE_FDOPENDIR
#define HAVE_FSTATAT
#define HAVE_GETADDRINFO
#define HAVE_GETCWD
#define HAVE_GETENV
#define HAVE_GETHOSTBYADDR
#define HAVE_GETHOSTBYNAME
#define HAVE_GETHOSTNAME
#define HAVE_GETNETBYNAME
#define HAVE_GETPID
#define HAVE_GETPPID
#define HAVE_GETPROTOBYNAME
#define HAVE_GETPROTOBYNUMBER
#define HAVE_GETPWNAM
#define HAVE_GETRUSAGE
#define HAVE_GETSERVBYNAME
#define HAVE_GETSERVBYPORT
#define HAVE_HSTRERROR
#define HAVE_IOCTL
#define HAVE_LINK
#define HAVE_LINKAT
#define HAVE_MALLOC
#define HAVE_MEMMOVE
#define HAVE_MKDIR
#define HAVE_MKDIRAT
#define HAVE_MKFIFO
#define HAVE_MKFIFOAT
#define HAVE_MMAP
#define HAVE_NANOSLEEP
#define HAVE_OPEN
#define HAVE_OPENAT
#define HAVE_OPENDIR
#define HAVE_OPENPTY
#define HAVE_PIPE
#define HAVE_READLINK
#define HAVE_READLINKAT
#define HAVE_REMOVE
#define HAVE_RENAME
#define HAVE_RMDIR
#define HAVE_SELECT
#define HAVE_SNPRINTF
#define HAVE_SOCKET
#define HAVE_STAT
#define HAVE_STRERROR
#define HAVE_SYMLINK
#define HAVE_SYMLINKAT
#define HAVE_SYSCONF
#define HAVE_SYSCTL
#define HAVE_SYSLOG
#define HAVE_TCGETSETATTR
#define HAVE_UNLINK
#define HAVE_UNLINKAT
#define HAVE_UTIMES
#define HAVE_WAITPID

#define HAVE_EMSCRIPTEN_GET_NOW_not

/*---------------------------------------------------------------------------*/
