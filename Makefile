CONFIGDIR=../config
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/perl/defs.mk
include $(CONFIGDIR)/saxon/defs.mk

ONTONOTES=/p/nl/corpora/ldc/ontonotes-release-3_0
VERBNET=/u/wdebeaum/dsl/verbnet-3.2
# TODO detect version numbers and pass as XSL parameters so we can include them in provenance instead of hardcoding them

Data: Data/OntoNotes Data/VerbNet

Data/OntoNotes: Data/OntoNotes/sense-inventories Data/OntoNotes/frames

Data/OntoNotes/sense-inventories: Code/converters/ontonotes-inventory-to-dsl.xsl
	mkdir -p $@
	for f in $(ONTONOTES)/data/english/metadata/sense-inventories/*.xml ; do \
	cat $$f \
	  | grep -v 'DOCTYPE\|SYSTEM\|\\<?xml' \
	  | $(JAVA) -jar $(SAXON_JAR) -xsl:$< -s:- \
	      -o:$@/`echo $$f |sed -e 's/.*\///; s/\.xml$$/.lisp/;'` ; \
	done

Data/OntoNotes/frames: Code/converters/ontonotes-frameset-to-dsl.xsl
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

Data/VerbNet: Code/converters/verbnet-to-dsl.xsl
	mkdir -p $@
	for f in $(VERBNET)/*.xml ; do \
	  $(JAVA) -jar $(SAXON_JAR) -xsl:$< \
	    -s:$$f -o:$@/`echo $$f |sed -e 's/.*\///; s/\.xml$$/.lisp/;'` ; \
	done

