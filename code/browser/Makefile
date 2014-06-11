# Makefile for DSL ontology browser
# William de Beaumont
# 2014-06-11

CONFIGDIR = ../../../config
include $(CONFIGDIR)/defs.mk
TRIPS_BASE = $(prefix)
DATA_BUILD_DIR = ../../data/browser
WWW_DIR = $(prefix)/etc/ont-browser

export TRIPS_BASE

all:: write-trips-ont-xmls.sh write-trips-ont-xmls.lisp
	mkdir -p $(DATA_BUILD_DIR)
	./write-trips-ont-xmls.sh $(DATA_BUILD_DIR)

install:: all ont-type.xsl exslt/str.tokenize.template.xsl
	$(MKINSTALLDIRS) $(WWW_DIR)
	$(INSTALL_DATA) \
		$(DATA_BUILD_DIR)/*.xml \
		ont-type.xsl \
		exslt/str.tokenize.template.xsl \
		$(WWW_DIR)

clean::
	rm -rf $(DATA_BUILD_DIR)

