CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml -I./
OCAMLOPT = ocamlfind ocamlopt -package "stdext,unix,str"

filesystem_summarise: filesystem_summarise.cmx filesystem_summarise_stubs.o
	$(OCAMLOPT) -linkpkg filesystem_summarise.cmx filesystem_summarise_stubs.o -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

install: filesystem_summarise bugtool/filesystem_summarise.xml bugtool/stuff.xml whitelist
	install -D filesystem_summarise $(DESTDIR)/opt/xensource/libexec/filesystem_summarise
	install -D bugtool/filesystem_summarise.xml $(DESTDIR)/etc/xensource/bugtool/filesystem_summarise.xml
	install -D bugtool/stuff.xml $(DESTDIR)/etc/xensource/bugtool/filesystem_summarise/stuff.xml
	install -D 05-filesystem-summarise $(DESTDIR)/etc/firstboot.d/05-filesystem-summarise
	install -D whitelist $(DESTDIR)/etc/xensource/whitelist

RPM_SOURCEDIR?=/usr/src/redhat/SOURCES
RPM_RELEASE=$(call git rev-list HEAD | wc -l)
.PHONY: version.inc 
version.inc:
	echo -n 'RPM_RELEASE=' > $@
	git rev-list HEAD | wc -l >> $@
include version.inc
filesystem_summarise.spec: filesystem_summarise.spec.in version.inc
	sed -e 's/@RPM_RELEASE@/$(RPM_RELEASE)/g' < $< > $@

srpm: filesystem_summarise.spec version.inc
	git archive --prefix=filesystem_summarise-0/ --format=tar HEAD | bzip2 -z > $(RPM_SOURCEDIR)/filesystem_summarise.tar.bz2
	rpmbuild -bs --nodeps filesystem_summarise.spec

