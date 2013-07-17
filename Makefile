SUBDIRS=code data

all install clean:
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@) || exit 1; \
	  fi; \
	done

