
fs: fs.ml
	ocamlfind ocamlopt -linkpkg -package "stdext,unix,str" fs.ml -o fs

watch: watch.ml
	ocamlfind ocamlopt -linkpkg -package "inotify,unix" watch.ml -o watch

install: fs bugtool/fs.xml bugtool/stuff.xml
	install -D fs $(DESTDIR)/usr/bin/fs
	install -D bugtool/fs.xml $(DESTDIR)/etc/xensource/bugtool/fs.xml
	install -D bugtool/stuff.xml $(DESTDIR)/etc/xensource/bugtool/fs/stuff.xml
	install -D 05-filesystem-check $(DESTDIR)/etc/firstboot.d
