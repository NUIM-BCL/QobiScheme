AC_PREREQ([2.67])
AC_INIT([QobiScheme], [1.44], [qobi@purdue.edu])
AC_CANONICAL_SYSTEM
AC_CONFIG_SRCDIR([source/jdhuff.h])
dnl AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_RANLIB
AC_CHECK_PROG([AR], [ar], [ar])
AC_CHECK_PROG([SCC], [scc], [scc], [reject])
AC_CHECK_PROG([SCH], [sch], [sch], [reject])

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lX11':
AC_CHECK_LIB([X11], [XHostAddress])

# Checks for header files.
AC_PATH_X
AC_CHECK_HEADERS([fcntl.h limits.h malloc.h memory.h netinet/in.h stddef.h stdlib.h string.h strings.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([bzero floor ftime getcwd gettimeofday memset pow select socket sqrt strrchr])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
