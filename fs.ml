
open Stringext

module File = struct
	type kind = File | Dir
	let string_of_kind = function
		| File -> "File"
		| Dir -> "Dir"
	let kind_of_string = function
		| "File" -> File
		| "Dir" -> Dir
		| x -> failwith (Printf.sprintf "Unknown kind: '%s'" x)
	type t = {
		path: string;
		mtime: int64;
		size: int64;
		kind: kind;
	}
	let of_stat path s =
		{ 
			path = path; 
			mtime = Int64.of_float s.Unix.LargeFile.st_mtime; 
			size = s.Unix.LargeFile.st_size;
			kind = match s.Unix.LargeFile.st_kind with
				| Unix.S_REG -> File
				| Unix.S_DIR -> Dir
				| _ -> failwith "Illegal st_kind"
		}

	let of_string line =
		match List.rev (String.split ',' line) with
			| mtime :: size :: kind :: rest ->
				let path = String.concat "," (List.rev rest) in
				{ 
					mtime = Int64.of_string mtime;
					size = Int64.of_string size;
					path = path;
					kind = kind_of_string kind;
				}
			| _ ->
				raise Not_found
	let to_string x =
		Printf.sprintf "%s,%s,%Ld,%Ld\n" x.path (string_of_kind x.kind) x.size x.mtime
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

let to_file index filename = 
	Unixext.with_file filename [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 
		(fun fd ->
			StringMap.iter
				(fun path file ->
					let s = File.to_string file in
					ignore(Unix.write fd s 0 (String.length s))
				) index
		)

let of_file = 
	Unixext.file_lines_fold
		(fun index line ->
			try
				let file = File.of_string line in
				StringMap.add file.File.path file index
			with Not_found ->
				Printf.fprintf stderr "Skipping line: %s\n" line;
				index
		) StringMap.empty 

let diff a b = 
	StringMap.partition
		(fun path file ->
			StringMap.mem path b && (StringMap.find path b = file)
		) a

let stats a = 
	StringMap.fold
		(fun path file (nfiles, nbytes) -> 
			if file.File.kind = File.File
			then Int64.(add nfiles 1L, add nbytes file.File.size)
			else (nfiles, nbytes)
		) a (0L, 0L)

let string_of_stats (nfiles, nbytes) = Printf.sprintf "%Ld files (%Ld KiB)" nfiles (Int64.div nbytes 1024L)			

let show_files prefix = 
	StringMap.iter
		(fun path file ->
			if file.File.kind = File.File
			then Printf.printf "%s%s\n" prefix path)

type whitelist_kind = 
		| Ignore  (** harmless mutation: ignore these *)
		| Trash   (** rubbish which should be deleted on boot *)
		| Persist (** configuration data which must be preserved *)

let whitelist_kind_of_string = function
	| "I" -> Ignore
	| "T" -> Trash
	| "P" -> Persist
	| x -> failwith (Printf.sprintf "Unknown whitelist_kind: %s" x)

open Listext

let whitelist_of_file filename = 
	let lines = Unixext.file_lines_fold 
		(fun acc line -> 
			let line = String.strip String.isspace line in
			if line = "" || (String.startswith "#" line) then acc
			else begin
				match String.split ':' ~limit:2 line with
					| (("I" | "T" | "P" ) as key) :: r :: [] ->
						(* Check r is itself a valid regexp *)
						begin 
							try let (_:Str.regexp) = Str.regexp r in ()
							with e ->
								Printf.fprintf stderr "Failed to parse whitelist file line: %s\n" line;
								raise e
						end;
						(whitelist_kind_of_string key, r) :: acc
					| _ -> 
						failwith (Printf.sprintf "Failed to parse whitelist file line: %s\n" line)
			end
		) [] filename in
	let regexp_of_lines = function
		| [] -> None
		| lines -> 
			let r = "\\(" ^ (String.concat "\\|" lines) ^ "\\)" in
			Some (Str.regexp r) in
	let one kind =
		kind, regexp_of_lines (List.filter_map (fun (key, line) -> if key = kind then Some line else None) lines) in
	[ one Ignore; one Trash; one Persist ]


let diff_whitelist a r = 
	StringMap.partition
		(fun path file ->
			match r with
				| None -> false
				| Some r -> Str.string_match r path 0
		) a

type mode = 
		| Summarise
		| Delete
		| List

let _ =
	let path = ref "/" in
	let db = ref "/var/xapi/files.db" in
	let whitelist = ref "/etc/xensource/whitelist" in
	let mode = ref Summarise in
	let verbose = ref false in
	Arg.parse
		[ 
			"-path", Arg.Set_string path, Printf.sprintf "Path to scan (default %s)" !path;
			"-db", Arg.Set_string db, Printf.sprintf "Path to database (default %s)" !db;
			"-whitelist", Arg.Set_string whitelist, Printf.sprintf "Path to whitelist (default %s)" !whitelist;
			"-mode", Arg.Symbol ([ "summarise"; "delete"; "list" ], function
					| "summarise" -> mode := Summarise
					| "delete" -> mode := Delete
					| "list" -> mode := List
					| _ -> assert false), "Mode (default: summarise)";
			"-v", Arg.Set verbose, Printf.sprintf "Verbose (default %b)" !verbose;
		]
		(fun x -> Printf.fprintf stderr "Skipping unknown argument: %s\n%!" x)
		"Scan for modified files in a filesystem";

	if Sys.file_exists !db then begin
		let whitelist = 
			if Sys.file_exists !whitelist 
			then whitelist_of_file !whitelist 
			else [ Ignore, None; Persist, None; Trash, None ] in
		let previous = of_file !db in
		let current = of_dir !path in
		let _, modified = diff previous current in
		let _, created = diff current previous in
		let different = StringMap.merge
			(fun path a b -> match a, b with
				| Some f, _ -> Some f
				| _, Some f -> Some f
				| None, None -> None) modified created in

		let ignore, rest = diff_whitelist different (List.assoc Ignore whitelist) in
		let trash, rest = diff_whitelist rest (List.assoc Trash whitelist) in
		let persist, rest = diff_whitelist rest (List.assoc Persist whitelist) in
			
		match !mode with
			| Summarise ->
				Printf.printf "Total:             %s\n" (string_of_stats (stats different));
				Printf.printf "Whitelist/Ignore:  %s\n" (string_of_stats (stats ignore));
				if !verbose then show_files "    " ignore;
				Printf.printf "Whitelist/Trash:   %s\n" (string_of_stats (stats trash));
				if !verbose then show_files "    " trash;
				Printf.printf "Whitelist/Persist: %s\n" (string_of_stats (stats persist));
				if !verbose then show_files "    " persist;				
				Printf.printf "Unexpected:        %s\n" (string_of_stats (stats rest));
				show_files "    " rest;
				
				if rest <> StringMap.empty then exit 1;
				exit 0;
			| List ->
				show_files "" rest
			| Delete -> failwith "Unimplemented"
	end else begin
		Printf.printf "Creating initial database\n";
		let current = of_dir !path in
		to_file current !db
	end

