type test = { name : string; fs : (string * (unit -> unit)) list }
(** Described a comparison benchmarks : we want to compare the
   performances of [fs] functions, named [name]. *)

(*******************************************************)

(** Benchmarks for single function. *)
let add_empty_header (module H : Headers.HEADERS) () =
  ignore H.(add (init ()) "transfer-encoding" "chunked")

let add_long_header (type a) (module H : Headers.HEADERS with type t = a)
    (h : a) () =
  ignore H.(add h "transfer-encoding" "chunked")

let get (type a) (module H : Headers.HEADERS with type t = a) (h : a) () =
  ignore H.(get h "transfer-encoding")

(*******************************************************)
(** Functions to build the list of benchmarked functions *)

module Mmap : Headers.HEADERS = Headers.HeadersMap

module Massoc : Headers.HEADERS = Headers.HeadersAssoc

module MassocAlt : Headers.HEADERS = Headers.HeadersAssocAlt

let pre_build_headers () =
  let build (type a) (module H : Headers.HEADERS with type t = a) : a =
    let h = H.init () in
    let h = H.add h "transfer-encoding" "chunked" in
    let h = H.add h "accept" "application/xml" in
    h
  in
  (build (module Mmap), build (module Massoc), build (module MassocAlt))

let pre_build_longheaders () =
  let build (type a) (module H : Headers.HEADERS with type t = a) : a =
    let rec add_headers n h =
      match n with
      | 0 -> h
      | n ->
          let k = Printf.sprintf "h%d" n in
          let v = Printf.sprintf "v%d" n in
          let h = H.add h k v in
          add_headers (n - 1) h
    in
    let h = add_headers 20 (H.init ()) in
    let h = H.add h "transfer-encoding" "chunked" in
    let h = add_headers 20 h in
    h
  in
  (build (module Mmap), build (module Massoc), build (module MassocAlt))

let build_comparison (f : (module Headers.HEADERS) -> unit -> unit) =
  let fmap = f (module Headers.HeadersMap) in
  let fassoc = f (module Headers.HeadersAssoc) in
  let fassocalt = f (module Headers.HeadersAssocAlt) in
  [ ("map", fmap); ("assoc", fassoc); ("assocalt", fassocalt) ]

(* Trick to be able to use a input polymorphic function with different
   type; this way, the forall is inside the record. *)
type packed = {
  f : 'a. (module Headers.HEADERS with type t = 'a) -> 'a -> unit -> unit;
}

let build_comparison_with_prebuilt_headers ({ f } : packed) builder =
  let hmap, hassoc, hassocalt = builder () in
  let fmap = f (module Mmap) hmap in
  let fassoc = f (module Massoc) hassoc in
  let fassocalt = f (module MassocAlt) hassocalt in
  [ ("map", fmap); ("assoc", fassoc); ("assocalt", fassocalt) ]

let tests : test list =
  [
    { name = "Add"; fs = build_comparison add_empty_header };
    {
      name = "Add2";
      fs =
        build_comparison_with_prebuilt_headers { f = add_long_header }
          pre_build_longheaders;
    };
    (*{
      name = "Add3";
      fs = build_comparison add_empty_header;
    };
    {
      name = "get";
      fs = build_comparison_with_prebuilt_headers { f = get } pre_build_headers;
    };*)
  ]
