
fs: fs.ml
	ocamlfind ocamlopt -linkpkg -package "stdext,unix,str" fs.ml -o fs

watch: watch.ml
	ocamlfind ocamlopt -linkpkg -package "inotify,unix" watch.ml -o watch