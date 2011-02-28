
module File = struct
	type t = {
		path: string;
		mtime: int;
	}
	let of_stat path s =
		{ 
			path = path; 
			mtime = int_of_float s.Unix.LargeFile.st_mtime; 
		}
end

module StringMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

type index = File.t StringMap.t

let of_dir (root: string) = 
	let rec f (root: string) (st_dev, inodes, files) = 
		let s = Unix.LargeFile.lstat root in

		let seen_before = IntSet.mem s.Unix.LargeFile.st_ino inodes in
		let file_or_dir = 
			false
			|| s.Unix.LargeFile.st_kind = Unix.S_REG
			|| s.Unix.LargeFile.st_kind = Unix.S_DIR in
		let on_this_device = s.Unix.LargeFile.st_dev = st_dev in
		if file_or_dir && on_this_device && not(seen_before)
		then 
			let children = 
				if Sys.is_directory root then Sys.readdir root else [| |] in
			let files = StringMap.add root (File.of_stat root s) files in
			let inodes = IntSet.add s.Unix.LargeFile.st_ino inodes in
			Array.fold_left 
				(fun acc file -> 
					f (Filename.concat root file) acc) 
				(st_dev, inodes, files) children
		else (st_dev, inodes, files) in
	let s = Unix.LargeFile.lstat root in
	let _, _, files = 
		f root (s.Unix.LargeFile.st_dev, IntSet.empty, StringMap.empty) in
	files

open Stringext

let to_fd index fd = 
	StringMap.iter
		(fun path file ->
			let s = Printf.sprintf "%s,%d\n" (String.escaped ~rules:[',', "\\,"] path) file.File.mtime in
			ignore(Unix.write fd s 0 (String.length s))
		) index

let _ =
	let stat = Unix.LargeFile.stat "/" in
	let all = of_dir "/" in
	Unixext.with_file "/tmp/files.db" [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 
		(to_fd all)
