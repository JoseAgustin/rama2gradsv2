#
# This file is processed by automake (produces file Makefile.in)
#
# Compiler flags -I$(top_srcdir)
FCFLAGS = -I$(top_srcdir)
AM_CPPFLAGS=-O2
if GFOR
AM_CPPFLAGS += -DGFOR -fdec
FCFLAGS += -fdec
endif

TESTS = test1 test2 test3 test4 test5 test6 test7 test8
TESTS += test9 test10

check_PROGRAMS= test1 test2 test3 test4 test5 test6 test7 test8
check_PROGRAMS += test9 test10

test1_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_logs.F90
test2_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_vconv.F90
test3_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_num2c.F90
test4_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_hyr.F90
test5_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_esta.F90
test6_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_cuenta.F90
test7_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_lee_est.F90
test8_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_simat.F90
test9_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_lee_nml.F90
test10_SOURCES= $(top_builddir)/mod_rama2grads.F90 t_output.F90

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod package.m4 testsuite

# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
	echo '# Signature of the current package.' && \
	echo 'm4_define([AT_PACKAGE_NAME],' && \
	echo '  [@PACKAGE_NAME@])' && \
	echo 'm4_define([AT_PACKAGE_TARNAME],' && \
	echo '  [@PACKAGE_TARNAME@])' && \
	echo 'm4_define([AT_PACKAGE_VERSION],' && \
	echo '  [@PACKAGE_VERSION@])' && \
	echo 'm4_define([AT_PACKAGE_STRING],' && \
	echo '  [@PACKAGE_STRING@])' && \
	echo 'm4_define([AT_PACKAGE_BUGREPORT],' && \
	echo '  [@PACKAGE_BUGREPORT@])'; \
	echo 'm4_define([AT_PACKAGE_URL],' && \
	echo '  [@PACKAGE_URL@])'; \
	}>'$(srcdir)/package.m4'

TESTSUITE = $(srcdir)/testsuite

EXTRA_DIST = testsuite.at $(srcdir)/package.m4 $(TESTSUITE)
EXTRA_DIST += est_rama.txt pol_data.csv met_data.csv namelist.nml     
check-local: atconfig  $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

maintainer-clean-local:
	rm -rf aclocal.m4 configure Makefile.in autotools m4
     
installcheck-local: atconfig  $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)
     
clean-local:
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = $(SHELL) $(top_builddir)/autoconf/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
