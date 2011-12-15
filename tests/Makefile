#
# This file is part of Barista.
# Copyright (C) 2007-2011 Xavier Clerc.
#
# Barista is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Barista is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

JAVA=java
JAVAC=javac
JAVAP=javap
JAVA_VERSION=1.6
TEST_CLASSPATH=$(shell test -x /usr/libexec/java_home && echo ".:`/usr/libexec/java_home`/../Classes/classes.jar" || echo ".:$(CLASSPATH)")
ANT=ant
EXEC_PATH=../../_build/src/driver

default: FORCE
	@echo 'Available targets:'
	@echo '  all              runs all tests'
	@echo '  one NAME=dir     runs the test whose name is passed'
	@echo '  clean            deletes log and result files'
	@echo '  log              print log for last execution'

all: clean prepare
	@for dir in `ls -d [0-9]*`; do \
	  if [ -d $$dir ]; then \
	    $(MAKE) one NAME=$$dir; \
	  fi \
	done 2>&1 | tee _log
	@$(MAKE) report

one: FORCE
	@if [ -z $(NAME) ]; then echo "*** value for 'NAME' is missing"; exit 1; fi
	@echo "Running tests for '$(NAME)' ...\c"
	@if [ -f $(NAME)/Makefile.tool ]; then \
	  echo " tool\c"; \
	  cd $(NAME); \
	  $(MAKE) -f Makefile.tool EXEC=$(EXEC_PATH)/barista.byte JAVA=$(JAVA) JAVAC=$(JAVAC) JAVAP=$(JAVAP) CLASSPATH=$(TEST_CLASSPATH); \
	  $(MAKE) -f Makefile.tool EXEC=$(EXEC_PATH)/barista.native JAVA=$(JAVA) JAVAC=$(JAVAC) JAVAP=$(JAVAP) CLASSPATH=$(TEST_CLASSPATH); \
	  (test -f $(EXEC_PATH)/barista.jar && $(MAKE) -f Makefile.tool EXEC='$(JAVA) -Xmx2G -jar $(EXEC_PATH)/barista.jar' JAVA=$(JAVA) JAVAC=$(JAVAC) JAVAP=$(JAVAP) CLASSPATH=$(TEST_CLASSPATH)) || true; \
	fi
	@if [ -f $(NAME)/Makefile.ocaml ]; then \
	  echo " ocaml\c"; \
	  cd $(NAME); \
	  $(MAKE) -f Makefile.ocaml COMP=ocamlc LIB_EXT=cma PROG_EXT=bc JAVAC=$(JAVAC) JAVAP=$(JAVAP) RUN_JAVA='$(JAVA) -Xmx2G -cp .'; \
	  $(MAKE) -f Makefile.ocaml COMP=ocamlopt LIB_EXT=cmxa PROG_EXT=opt JAVAC=$(JAVAC) JAVAP=$(JAVAP) RUN_JAVA='$(JAVA) -Xmx2G -cp .'; \
	  (test -f $(EXEC_PATH)/barista.jar && $(MAKE) -f Makefile.ocaml COMP=ocamljava LIB_EXT=cmja PROG_EXT=jar FLAGS='' RUN_PROG='$(JAVA) -Xmx2G -jar' JAVAC=$(JAVAC) JAVAP=$(JAVAP) RUN_JAVA='java -cp .') || true; \
	fi
	@echo ""

clean: 
	@for dir in `ls -d [0-9]*`; do \
	  if [ -d $$dir ]; then \
	    $(MAKE) clean_one NAME=$$dir; \
	  fi \
	done 2>&1

clean_one: FORCE
	@if [ -z $(NAME) ]; then echo "*** value for 'NAME' is missing"; exit 1; fi
	@echo "Cleaning '$(NAME)' ..."
	@ls $(NAME)
	@rm -f $(NAME)/_log
	@rm -f $(NAME)/*.cm*
	@rm -f $(NAME)/*.jo
	@rm -f $(NAME)/*.o
	@ls $(NAME)
	@$(MAKE) -f Makefile.ocaml -C $(NAME) clean || true

log: FORCE
	@cat _log
	@$(MAKE) report

report: FORCE
	@echo ''
	@echo "`grep '^make[3].*\*\*\*' _log | wc -l` error(s) in `grep '^Running tests for' _log | wc -l` test(s)"

prepare:
	@javac -target $(JAVA_VERSION) -d classes classes/*.java

FORCE:
