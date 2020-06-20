#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([rama2gradsv2], [v1.4], [agustin@atmosfera.unam.mx], ,[https://github.com/JoseAgustin/rama2gradsv2])
# Define our M4 macro directory
AC_CONFIG_AUX_DIR([autoconf])
AM_INIT_AUTOMAKE([1.11 foreign])
# This is the default top directory of the installation
AC_PREFIX_DEFAULT(`pwd`)

# Checks for programs.
AC_PROG_LN_S
AC_PROG_MAKE_SET
# Set language for configuration checks
AC_LANG(Fortran)
AC_PROG_FC
AC_FC_FREEFORM
# Checks for libraries.
AC_OPENMP

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
# Check option "--enable-parallel"
MSG_PARALLEL="(serial)"
AC_ARG_ENABLE([parallel],
  [AS_HELP_STRING([--enable-parallel],[turn on parallel execution (with OPENMP)])],
  [case "${enableval}" in
    yes) parallel=true ;;
     no) parallel=false ;;
      *) AC_MSG_ERROR([bad value ${enableval} for --enable-parallel]) ;;
   esac], [parallel=true])
   if test "${parallel}" = "true"; then
    FCFLAGS="${OPENMP_FCFLAGS} ${FCFLAGS}";
   else
    FCFLAGS="${FCFLAGS} ";
    fi

AC_CONFIG_FILES([Makefile])

AC_OUTPUT

AC_MSG_NOTICE([---------------------------------------------------------])
AC_MSG_NOTICE([Configuration complete - $PACKAGE_NAME-$PACKAGE_VERSION $MSG_PARALLEL])
AC_MSG_NOTICE([])
AS_IF([test "x$NC_VERSION" != "x"],AC_MSG_NOTICE([Using $NC_VERSION]))
AC_MSG_NOTICE([Fortran compiler:           FC=$FC])
AS_IF([test "$parallel" = "true"],
AC_MSG_NOTICE([Enable parallel version:    --enable-parallel=yes]),
AC_MSG_NOTICE([Enable parallel version:    --enable-parallel=no]))
AC_MSG_NOTICE([Fortran flags:              FCFLAGS=$FCFLAGS])
AC_MSG_NOTICE([Install prefix:             --prefix=$prefix])
AC_MSG_NOTICE([Executables install prefix: --exec_prefix=$exec_prefix])
AC_MSG_NOTICE([Binary directory:           --bindir=$bindir])
