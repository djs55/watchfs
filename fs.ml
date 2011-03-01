
module File = struct
	type t = {
		path: string;
		mtime: int64;
	}
	let of_stat path s =
		{ 
			path = path; 
			mtime = Int64.of_float s.Unix.LargeFile.st_mtime; 
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

let to_file index filename = 
	Unixext.with_file filename [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 
		(fun fd ->
			StringMap.iter
				(fun path file ->
					let s = Printf.sprintf "%s,%Ld\n" path file.File.mtime in
					ignore(Unix.write fd s 0 (String.length s))
				) index
		)

let of_file = 
	Unixext.file_lines_fold
		(fun index line ->
			match List.rev (String.split ',' line) with
				| mtime :: rest ->
					let path = String.concat "," (List.rev rest) in
					let file = { 
						File.mtime = Int64.of_string mtime;
						File.path = path
					} in
					StringMap.add path file index
				| _ ->
					Printf.fprintf stderr "Skipping line: %s\n" line;
					index
		) StringMap.empty 

let diff a b = 
	StringMap.partition
		(fun path file ->
			StringMap.mem path b && (StringMap.find path b = file)
		) a

let _ =
	let path = ref "/root" in
	let db = ref "/home/djs/files.db" in
	Arg.parse
		[ 
			"-path", Arg.Set_string path, Printf.sprintf "Path to scan (default %s)" !path;
			"-db", Arg.Set_string db, Printf.sprintf "Path to database (default %s)" !db;
		]
		(fun x -> Printf.fprintf stderr "Skipping unknown argument: %s\n%!" x)
		"Scan for modified files in a filesystem";

	if Sys.file_exists !db then begin
		let previous = of_file !db in
		let current = of_dir !path in
		let same, different = diff previous current in
		if different <> StringMap.empty then begin
			Printf.printf "There are unexpected filesystem differences:\n";
			StringMap.iter (fun path file -> 
				Printf.printf "     %Ld %s\n" file.File.mtime path) different;
			exit 1;
		end else begin
			Printf.printf "No unexpected filesystem differences.\n";
			exit 0;
		end
	end else begin
		Printf.printf "Creating initial database\n";
		let current = of_dir !path in
		to_file current !db
	end
