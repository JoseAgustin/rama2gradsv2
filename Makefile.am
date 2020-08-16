#
# This file is processed by automake (produces file Makefile.in)
#
ACLOCAL_AMFLAGS = -I autoconf

# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/testsuite/package.m4: $(top_srcdir)/configure.ac

bin_PROGRAMS = rama2grads.exe

rama2grads_exe_SOURCES = mod_rama2grads.F90 rama2grads.F90

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod 

TESTSUITE = $(srcdir)/testsuite

SUBDIRS = . testsuite

EXTRA_DIST = README.md $(TESTSUITE)

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
