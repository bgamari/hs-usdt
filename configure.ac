AC_PREREQ([2.60])
AC_INIT([Haskell usdt package], [1.0], [ben@smart-cactus.org], [hs-usdt])

AC_PROG_CC

AC_CHECK_PROGS([DTRACE], [dtrace])
AC_CHECK_HEADERS([sys/sdt.h])
if test -n "$DTRACE"; then
    USDT_CPPFLAGS="-DDTRACE=\"$DTRACE\""
else
    USDT_CPPFLAGS=""
fi

AC_SUBST([USDT_CPPFLAGS])
AC_CONFIG_FILES([hs-usdt.buildinfo])
AC_OUTPUT
