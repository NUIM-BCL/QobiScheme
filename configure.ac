#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Prologue
AC_PREREQ([2.67])
AC_INIT([QobiScheme],[1.46],[qobi@purdue.edu])
AC_CANONICAL_TARGET
AC_CONFIG_SRCDIR([source/QobiScheme.sc])
dnl AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_RANLIB
AC_CHECK_PROG([AR], [ar], [ar])
AC_CHECK_PROG([SCC], [scc], [scc], [false])
AC_CHECK_PROG([SCH], [sch], [sch], [false])

# Checks for libraries.
AC_CHECK_LIB([X11], [XOpenDisplay])
AC_CHECK_LIB([sigsegv], [sigsegv_install_handler])

AC_ARG_WITH([scxl], [AS_HELP_STRING([--with-scxl], [use Scheme->C X11 libraries])], [], [with_scxl=/usr/lib/scheme2c])

SCXL=
AS_IF([test "x$with_scxl" != xno],
        [AC_SUBST([SCXL], [$with_scxl])])


# Checks for header files.
AC_PATH_X
AC_CHECK_HEADERS([fcntl.h limits.h malloc.h memory.h netinet/in.h stddef.h stdlib.h string.h strings.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([bzero floor ftime getcwd gettimeofday memset pow select socket sqrt strrchr])

# Epilogue
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
