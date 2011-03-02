CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml -I./
OCAMLOPT = ocamlfind ocamlopt -package "stdext,unix,str"

fs: fs.cmx fs_stubs.o
	$(OCAMLOPT) -linkpkg fs.cmx fs_stubs.o -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

install: fs bugtool/fs.xml bugtool/stuff.xml whitelist
	install -D fs $(DESTDIR)/opt/xensource/libexec/fs
	install -D bugtool/fs.xml $(DESTDIR)/etc/xensource/bugtool/fs.xml
	install -D bugtool/stuff.xml $(DESTDIR)/etc/xensource/bugtool/fs/stuff.xml
	install -D 05-filesystem-check $(DESTDIR)/etc/firstboot.d/05-filesystem-check
	install -D whitelist $(DESTDIR)/etc/xensource/whitelist

RPM_SOURCEDIR?=/usr/src/redhat/SOURCES
RPM_RELEASE=$(call git rev-list HEAD | wc -l)
.PHONY: version.inc 
version.inc:
	echo -n 'RPM_RELEASE=' > $@
	git rev-list HEAD | wc -l >> $@
include version.inc
fs.spec: fs.spec.in version.inc
	sed -e 's/@RPM_RELEASE@/$(RPM_RELEASE)/g' < $< > $@

srpm: fs.spec version.inc
	git archive --prefix=fs-0/ --format=tar HEAD | bzip2 -z > $(RPM_SOURCEDIR)/fs.tar.bz2
	rpmbuild -bs --nodeps fs.spec

