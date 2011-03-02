
fs: fs.ml
	ocamlfind ocamlopt -linkpkg -package "stdext,unix,str" fs.ml -o fs

watch: watch.ml
	ocamlfind ocamlopt -linkpkg -package "inotify,unix" watch.ml -o watch

install: fs bugtool/fs.xml bugtool/stuff.xml
	install -D fs $(DESTDIR)/usr/bin/fs
	install -D bugtool/fs.xml $(DESTDIR)/etc/xensource/bugtool/fs.xml
	install -D bugtool/stuff.xml $(DESTDIR)/etc/xensource/bugtool/fs/stuff.xml
	install -D 05-filesystem-check $(DESTDIR)/etc/firstboot.d/05-filesystem-check

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

