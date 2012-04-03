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

include Makefile.config

# PATHS

PATH_BASE=`pwd`
PATH_BUILD=$(PATH_BASE)/_build
PATH_OCAMLDOC=$(PATH_BASE)/ocamldoc
PATH_SRC=$(PATH_BASE)/src
PATH_INSTALL=$(PATH_OCAML_PREFIX)/lib/ocaml/barista


# DEFINITIONS

PROJECT_NAME=barista
OCAMLBUILD=$(PATH_OCAML_PREFIX)/bin/ocamlbuild
OCAMLBUILD_FLAGS=-classic-display -no-links -use-ocamlfind -cflags -annot
MODULES_ODOCL=$(PROJECT_NAME).odocl
MODULES_MLPACK=$(PROJECT_NAME)Library.mlpack


# TARGETS

default:
	@echo "available targets:"
	@echo "  all         compiles all files"
	@echo "  doc         generates ocamldoc documentations"
	@echo "  clean       deletes all produced files (excluding documentation)"
	@echo "  veryclean   deletes all produced files (including documentation)"
	@echo "  install     copies executable and library files"
	@echo "  generate    generates files needed for build"
	@echo "  scrap       generates small test files"

all: generate
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)Library.otarget

doc:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME).docdir/index.html
	cp $(PATH_BUILD)/$(PROJECT_NAME).docdir/*.html $(PATH_BUILD)/$(PROJECT_NAME).docdir/*.css $(PATH_OCAMLDOC)

scrap:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) test.native

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -clean
	rm -f $(MODULES_ODOCL) $(MODULES_MLPACK) $(PROJECT_NAME)Library.itarget

veryclean: clean
	rm -f $(PATH_OCAMLDOC)/*.html $(PATH_OCAMLDOC)/*.css

install: all
	if [ -x "$(PATH_OCAMLFIND)" ]; then \
	  $(PATH_OCAMLFIND) query $(PROJECT_NAME) && $(PATH_OCAMLFIND) remove $(PROJECT_NAME) || true; \
	  $(PATH_OCAMLFIND) install $(PROJECT_NAME) META -optional $(PATH_BUILD)/$(PROJECT_NAME)Library.cm* $(PATH_BUILD)/$(PROJECT_NAME)Library.a $(PATH_BUILD)/$(PROJECT_NAME)Library.ja $(PATH_BUILD)/$(PROJECT_NAME)Library.o $(PATH_BUILD)/src/driver/$(PROJECT_NAME).byte $(PATH_BUILD)/src/driver/$(PROJECT_NAME).native $(PATH_BUILD)/src/driver/$(PROJECT_NAME).jar; \
	else \
	  mkdir -p $(PATH_INSTALL); \
	  for ext in cma cmxa cmja a ja; do \
	    test -f $(PATH_BUILD)/$(PROJECT_NAME)Library.$$ext && cp $(PATH_BUILD)/$(PROJECT_NAME)Library.$$ext $(PATH_INSTALL) || true; \
	  done; \
	fi

generate:
	echo '$(PROJECT_NAME)Library.cma' > $(PROJECT_NAME)Library.itarget
	(test -x $(PATH_OCAML_PREFIX)/bin/ocamlopt && echo '$(PROJECT_NAME)Library.cmxa' >> $(PROJECT_NAME)Library.itarget) || true
	(test -x $(PATH_OCAML_PREFIX)/bin/ocamljava && echo '$(PROJECT_NAME)Library.cmja' >> $(PROJECT_NAME)Library.itarget) || true

.PHONY: doc clean generate scrap
