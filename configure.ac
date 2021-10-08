# -*- Autoconf -*-
#
# guile-gdal --- FFI bindings for GDAL with extensions
# Copyright (c) 2021 Ahmet Artu Yildirim <ahmet@artulab.com>
#
# This file is part of guile-gdal.
#
# Guile-gdal is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# Guile-gdal is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-gdal.  If not, see
# <http://www.gnu.org/licenses/>.

AC_INIT(guile-gdal, 0.0.1)
AC_CONFIG_SRCDIR(gdal)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AC_CONFIG_MACRO_DIRS([m4])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR

if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guile' binary not found; please check your guile-2.x installation.])
fi

AX_LIB_GDAL

if test "x$GDAL_VERSION" = "x"; then
   AC_MSG_ERROR(['gdal' library not found; please check your gdal installation.])
fi


AC_ARG_WITH([libgdal-path],
  [AS_HELP_STRING([--with-libgdal-path=PATH], [PATH of gdal dynamic library])],
  [LIBGDAL_PATH="$withval"],
  [LIBGDAL_PATH="/usr/lib/libgdal.so"])

AC_MSG_CHECKING([for libgdal shared library path])
AC_MSG_RESULT([$LIBGDAL_PATH])
AC_SUBST([LIBGDAL_PATH])

AC_CONFIG_FILES([Makefile gdal/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
