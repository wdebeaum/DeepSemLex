# 

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/perl/defs.mk
include $(CONFIGDIR)/ruby/defs.mk
include $(CONFIGDIR)/saxon/defs.mk
include $(CONFIGDIR)/DeepSemLex/defs.mk

export TRIPS_BASE=$(prefix)

# TODO detect version numbers and pass as XSL parameters so we can include them in provenance instead of hardcoding them

all: OntoNotes VerbNet

OntoNotes: OntoNotes/sense-inventories OntoNotes/frames

OntoNotes/sense-inventories: code/converters/ontonotes-inventory-to-dsl.xsl
	mkdir -p $@
	for f in $(ONTONOTES)/data/english/metadata/sense-inventories/*.xml ; do \
	cat $$f \
	  | grep -v 'DOCTYPE\|SYSTEM\|\\<?xml' \
	  | $(JAVA) -jar $(SAXON_JAR) -xsl:$< -s:- \
	      -o:$@/`echo $$f |sed -e 's/.*\///; s/\.xml$$/.lisp/;'` ; \
	done

OntoNotes/frames: code/converters/ontonotes-frameset-to-dsl.xsl
	mkdir -p $@
	for f in $(ONTONOTES)/data/english/metadata/frames/*.xml ; do \
	  cat $$f \
	  | $(PERL) -p -e ' \
	      next if (/DOCTYPE|SYSTEM/); \
	      s{<rel>brayed</arg>}{<rel>brayed</rel>}; \
	      s/"vntheta="/" vntheta="/; \
	      s/<vnrole vnlcs=/<vnrole vncls=/; \
	    ' \
	  | $(JAVA) -jar $(SAXON_JAR) -xsl:$< -s:- \
	      -o:$@/`echo $$f |sed -e 's/.*\///; s/\.xml$$/.lisp/;'` ; \
	done

VerbNet: code/converters/verbnet-to-dsl.xsl
	mkdir -p $@
	for f in $(VERBNET)/*.xml ; do \
	  $(JAVA) -jar $(SAXON_JAR) -xsl:$< \
	    -s:$$f -o:$@/`echo $$f |sed -e 's/.*\///; s/\.xml$$/.lisp/;'` ; \
	done

WordNet: code/converters/wnsql-to-dsl.rb
	mkdir -p $@
	cd $@ ; $(RUBY) ../../$<

