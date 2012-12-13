(* discord.ml
   Package database program.
   Hrrrm.

   Simon Heath
   9/3/2007
*)

type package = {
    mutable pkgname : string;
    mutable pkgsegments : (string, string) Hashtbl.t;
    mutable pkglabels : string list
  }


let makePackage name = {
    pkgname = name;
    pkgsegments = Hashtbl.create 8;
    pkglabels = []
  }
;;

let getName pkg =
  pkg.pkgname
;;

let addSegment pkg segmentname segment =
  Hashtbl.add pkg.pkgsegments segmentname segment
;;

let getSegment pkg segmentname =
  Hashtbl.find pkg.pkgsegments segmentname
;;

let delSegment pkg segmentname =
  Hashtbl.remove pkg.pkgsegments segmentname
;;

let getAllSegments pkg =
  let concatSegments nm vl accm =
    accm ^ "\n\n" ^ seperator ^ nm ^ "\n\n" ^ vl
  in
  Hashtbl.fold concatSegments pkg.pkgsegments ""
;;

let getAllSegmentNames pkg =
  let accmNames name seg accm =
    name :: accm
  in
  Hashtbl.fold accmNames pkg.pkgsegments []
;;



let hasLabel pkg label =
  List.mem label pkg.pkglabels
;;

let addLabel pkg label =
  if hasLabel pkg label then
    ()
  else
    pkg.pkglabels <- label :: pkg.pkglabels
;;

let delLabel pkg label =
  pkg.pkglabels <- List.filter (fun x -> x <> label) pkg.pkglabels
;;

let getLabels pkg =
  pkg.pkglabels
;;


(* Right now, databases are just, you know, lists.
   I love abstraction.
*)

let makeDB () = 
  ref []
;;

let addPackage db pkg = 
  db := pkg :: !db
;;

let getPackage db pkgname =
  List.find (fun x -> x.pkgname = pkgname) !db
;;

let delPackage db pkgname =
  db := List.filter (fun x -> x.pkgname <> pkgname) !db
;;


(* I love quadratic complexity algorithms, too. *)
let getPackagesWithLabel db label =
  List.filter (fun x -> hasLabel x label) db
;;

let addLabelToPackage db pkgname label =
  addLabel (getPackage db pkgname) label
;;




(* Now, for the finale... *)
let dumpToFile db filename =
  let c = open_out_bin filename in
    Marshal.to_channel c db [];
    close_out c;
;;

let readFromFile filename =
  print_endline filename;
  let c = open_in_bin filename in
  let db = (Marshal.from_channel c : package list ref) in
    close_in c;
    db
;;
