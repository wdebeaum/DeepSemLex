CONFIGDIR=../../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/perl/defs.mk
include $(CONFIGDIR)/ruby/defs.mk
include $(CONFIGDIR)/saxon/defs.mk
include $(CONFIGDIR)/DeepSemLex/defs.mk

export TRIPS_BASE=$(prefix)

# TODO detect version numbers and pass as XSL parameters so we can include them in provenance instead of hardcoding them

RESOURCE_SUBDIRS=PropBank/frames OntoNotes/sense-inventories VerbNet WordNet

all: $(RESOURCE_SUBDIRS:%=%/COMPLETE)

clean:
	rm -rf $(RESOURCE_SUBDIRS)

$(RESOURCE_SUBDIRS):
	mkdir -p $@

ON_SIS=$(shell ls $(ONTONOTES)/data/english/metadata/sense-inventories/*.xml \
	       |$(PERL) -n -e 'print "$$1\n" if (/\/([^\/-]+-[nv])\.xml$$/);')

OntoNotes/sense-inventories/COMPLETE: $(ON_SIS:%=OntoNotes/sense-inventories/%.lisp)
	touch $@

OntoNotes/sense-inventories/%.lisp: $(ONTONOTES)/data/english/metadata/sense-inventories/%.xml ../code/converters/ontonotes-inventory-to-dsl.xsl OntoNotes/sense-inventories
	cat $< \
	| grep -v 'DOCTYPE\|SYSTEM\|\\<?xml' \
	| $(JAVA) -jar $(SAXON_JAR) \
	    -xsl:../code/converters/ontonotes-inventory-to-dsl.xsl -s:- -o:$@

PB_WORDS=$(shell ls $(ONTONOTES)/data/english/metadata/frames/*.xml \
		 |$(PERL) -n -e 'chomp; print "$$1\n" if (/\/([^\/\.]+)\.xml$$/ and -r);')

PropBank/frames/COMPLETE: $(PB_WORDS:%=PropBank/frames/%.lisp)
	touch $@

PropBank/frames/%.lisp: $(ONTONOTES)/data/english/metadata/frames/%.xml ../code/converters/prep-propbank-frameset.pl ../code/converters/ontonotes-frameset-to-dsl.xsl PropBank/frames
	  cat $< \
	  | $(PERL) ../code/converters/prep-propbank-frameset.pl \
	  | $(JAVA) -jar $(SAXON_JAR) \
	      -xsl:../code/converters/ontonotes-frameset-to-dsl.xsl -s:- -o:$@

VN_CLASSES=$(shell ls $(VERBNET)/*.xml \
		   |$(PERL) -n -e 'print "$$1\n" if (/\/([^\/\.]+)\.xml$$/);')

VerbNet/COMPLETE: $(VN_CLASSES:%=VerbNet/%.lisp)
	touch $@

VerbNet/%.lisp: $(VERBNET)/%.xml ../code/converters/verbnet-to-dsl.xsl
	$(JAVA) -jar $(SAXON_JAR) \
	  -xsl:../code/converters/verbnet-to-dsl.xsl -s:$< -o:$@

WordNet/COMPLETE: ../code/converters/wnsql-to-dsl.rb WordNet
	cd WordNet ; $(RUBY) ../../$<
	touch $@

