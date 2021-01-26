module Unitary (H : Headers.HEADERS) = struct
  open Utils

  (*These tests try as much as possible to tests each functions separately. *)
  let t_header =
    Alcotest.testable
      (fun fmt h ->
        Fmt.pf fmt "%a"
          (Fmt.list ~sep:(Fmt.sps 1)
             (Fmt.pair ~sep:Fmt.comma Fmt.string Fmt.string))
          (H.to_list h))
      (fun x y -> H.compare x y = 0)

  let hstr =
    [
      ("accept", "application/xml");
      ("transfer-encoding", "chunked");
      ("accept", "text/html");
      ("content-length", "100");
    ]

  let prebuilt = H.of_list hstr

  let hrev = List.rev hstr

  let to_list_rev h = List.rev (H.to_list h)

  let to_list_tests () =
    aessl "to_list (init ())" [] H.(to_list (init ()));
    aessl "to_list (add (init ()) k v" [ ("a", "a1") ]
      H.(to_list (add (init ()) "a" "a1"));
    aessl "to_list (of_list h) = h" hstr H.(to_list prebuilt)

  let is_empty_tests () =
    aeb "is_empty (init ())" true H.(is_empty (init ()));
    aeb "is_empty (add (init ()) k v" false
      H.(is_empty (add (init ()) "a" "a1"));
    aeb "is_empty (remove (add (init ()) k v) k)" true
      H.(is_empty (remove (add (init ()) "a" "a1") "a"))

  (* [init_with l] *)
  let init_with_tests () =
    aessl "init_with k v"
      [ ("transfer-encoding", "chunked") ]
      H.(to_list (init_with "traNsfer-eNcoding" "chunked"))

  let mem_tests () =
    aeb "mem (init ()) k = false" false H.(mem (init ()) "a");
    aeb "mem h k" true H.(mem prebuilt "accept");
    aeb "mem h k" true H.(mem prebuilt "content-length");
    aeb "mem h k" false H.(mem prebuilt "a")

  let add_tests () =
    aessl "add h k v"
      (hstr @ [ ("a", "a1") ])
      H.(to_list (add prebuilt "a" "a1"));
    aessl "add (add h k v) k v"
      (hstr @ [ ("a", "a1"); ("a", "a1") ])
      H.(to_list (add (add prebuilt "a" "a1") "a" "a1"));
    aessl "add (add h k' v') k v"
      (hstr @ [ ("a", "a1"); ("b", "b1") ])
      H.(to_list (add (add prebuilt "a" "a1") "b" "b1"))

  let get_tests () =
    aeso "get (add (init () k v) k" (Some "a1")
      H.(get (add (init ()) "a" "a1") "a");
    aeso "get (add h k v) k when mem h k = false" (Some "a1")
      H.(get (add prebuilt "a" "a1") "a");
    aeso "get (add h k v) k when mem h k = true" (Some "text/html")
      H.(get (add prebuilt "a" "a1") "accept");
    aeso "get (add (add h k v') k v) k = v" (Some "a2")
      H.(get (add (add prebuilt "a" "a1") "a" "a2") "a")

  (* [add_list h l] is h with l at the end. It is the same than
     adding each element in l one by one in order. *)
  let add_list_tests () =
    let l = [ ("a", "a1"); ("b", "b1") ] in
    aessl "add_list (init ()) []" [] H.(to_list (add_list (init ()) []));
    aessl "add_list (init ()) l" l H.(to_list (add_list (init ()) l));
    aessl "add_list h []" hstr H.(to_list (add_list prebuilt []));
    aessl "add_list h [k, v]"
      (hstr @ [ ("a", "a1") ])
      H.(to_list (add_list prebuilt [ ("a", "a1") ]));
    aessl "add_list h l" (hstr @ l) H.(to_list (add_list prebuilt l))

  let add_multi_tests () =
    let k, vals = ("a", [ "a1"; "a2"; "a3" ]) in
    let l = List.map (fun v -> ("a", v)) vals in
    aessl "add_multi (init ()) k []" [] H.(to_list (add_multi (init ()) k []));
    aessl "add_multi (init ()) k vals" l
      H.(to_list (add_multi (init ()) k vals));
    aessl "add_multi h k []" hstr H.(to_list (add_multi prebuilt k []));
    aessl "add_multi h k vals" (hstr @ l)
      H.(to_list (add_multi prebuilt k vals))

  let add_unless_exists_tests () =
    let k, v = ("a", "a1") in
    let k', v' = ("transfer-encoding", "chunked") in
    let k'', v'' = ("accept", "text/*") in
    aessl "add_unless_exists (init ()) k v" [ (k, v) ]
      H.(to_list (add_unless_exists (init ()) k v));
    aessl "add_unless_exists h k v when mem h k = false"
      (hstr @ [ (k, v) ])
      H.(to_list (add_unless_exists prebuilt k v));
    aessl "add_unless_exists h k v when mem h k = true)" hstr
      H.(to_list (add_unless_exists prebuilt k' v'));
    aessl "add_unless_exists h k v when mem h k = true)" hstr
      H.(to_list (add_unless_exists prebuilt k'' v''))

  let remove_tests () =
    aessl "remove (init ()) k" [] H.(to_list (remove (init ()) "accept"));
    aessl "remove (add (add (init ()) k v) k v) k" []
      H.(to_list (remove (add (add (init ()) "k" "v") "k" "v") "k"));
    aessl "remove h k when mem h k = false" hstr
      H.(to_list (remove prebuilt "a"));
    aessl "remove h k when mem h k = true"
      [
        ("accept", "application/xml");
        ("accept", "text/html");
        ("content-length", "100");
      ]
      H.(to_list (remove prebuilt "transfer-encoding"));
    aessl "remove h k when mem h k = true"
      [ ("transfer-encoding", "chunked"); ("content-length", "100") ]
      H.(to_list (remove prebuilt "accept"))

  let replace_tests () =
    let k, v, v' = ("a", "a1", "a2") in
    aessl "replace (init ()) k v" [ (k, v) ] H.(to_list (replace (init ()) k v));
    aessl "replace (add (init ()) k v) k v" [ (k, v) ]
      H.(to_list (replace (add (init ()) k v) k v));
    aessl "replace (add (init ()) k v) k v'" [ (k, v') ]
      H.(to_list (replace (add (init ()) k v) k v'));
    aessl "replace h k v when mem h = false"
      (hstr @ [ (k, v) ])
      H.(to_list (replace prebuilt k v));
    aessl "replace h k v when mem h = true"
      [
        ("accept", "application/xml");
        ("transfer-encoding", "gzip");
        ("accept", "text/html");
        ("content-length", "100");
      ]
      H.(to_list (replace prebuilt "transfer-encoding" "gzip"));
    aessl "replace h k v when mem h = true"
      [
        ("transfer-encoding", "chunked");
        ("accept", "text/*");
        ("content-length", "100");
      ]
      H.(to_list (replace prebuilt "accept" "text/*"))

  let update_tests () = ()

  let get_multi_tests () =
    aesl "get_multi (init ()) k" [] H.(get_multi (init ()) "a");
    aesl "get_multi h k when mem h k = false" [] H.(get_multi prebuilt "a");
    aesl "get_multi h k when mem h k = true" [ "chunked" ]
      H.(get_multi prebuilt "transfer-encoding");
    aesl "get_multi h k when mem h k = true"
      [ "application/xml"; "text/html" ]
      H.(get_multi prebuilt "accept")

  (* Change map and iter  type for (string -> string -> string) -> t ->
     t )*)
  let map_tests () =
    let a = ", a" in
    aessl "map (fun _ v -> v) (init ())" []
      H.(to_list (map (fun _k v -> v) (init ())));
    aessl "map (fun _ v -> v) (init ())" (H.to_list prebuilt)
      H.(to_list (map (fun _k v -> v) prebuilt));
    aessl "map (fun _ v -> v ^ a ) (init ())"
      [
        ("accept", "application/xml, a");
        ("transfer-encoding", "chunked, a");
        ("accept", "text/html, a");
        ("content-length", "100, a");
      ]
      H.(to_list (map (fun _k v -> v ^ a) prebuilt))

  let fold_tests () = ()

  let iter_tests () = ()

  let to_lines_tests () =
    aesl "to_lines h"
      [
        "accept: application/xml\r\n";
        "transfer-encoding: chunked\r\n";
        "accept: text/html\r\n";
        "content-length: 100\r\n";
      ]
      H.(to_lines prebuilt)

  let to_frames_tests () =
    aesl "to_frames h"
      [
        "accept: application/xml";
        "transfer-encoding: chunked";
        "accept: text/html";
        "content-length: 100";
      ]
      H.(to_frames prebuilt)

  let to_string_tests () =
    aes "to_string h"
      "accept: application/xml\r\n\
       transfer-encoding: chunked\r\n\
       accept: text/html\r\n\
       content-length: 100\r\n\
       \r\n"
      H.(to_string prebuilt)

  let tests name =
    [
      ( name ^ " - Unitary",
        [
          ("Header.to_list", `Quick, to_list_tests);
          ("Header.is_empty", `Quick, is_empty_tests);
          ("Header.init_with", `Quick, init_with_tests);
          ("Header.mem", `Quick, mem_tests);
          ("Header.add", `Quick, add_tests);
          ("Header.get", `Quick, get_tests);
          ("Header.add_list", `Quick, add_list_tests);
          ("Header.add_multi", `Quick, add_multi_tests);
          ("Header.add_unless_exists", `Quick, add_unless_exists_tests);
          ("Header.remove", `Quick, remove_tests);
          ("Header.replace", `Quick, replace_tests);
          ("Header.get_multi", `Quick, get_multi_tests);
          ("Header.to_lines", `Quick, to_lines_tests);
          ("Header.to_frames", `Quick, to_frames_tests);
          ("Header.to_string", `Quick, to_string_tests);
          ("Header.map", `Quick, map_tests);
          (*todo*)
          ("Header.update", `Quick, update_tests);
          ("Header.fold", `Quick, fold_tests);
          ("Header.iter", `Quick, iter_tests);
        ] );
    ]
end

module TestMap = Unitary (Headers.HeadersMap)
module TestAssoc = Unitary (Headers.HeadersAssoc)
module TestAssocAlt = Unitary (Headers.HeadersAssocAlt)

let tests =
  (*TestMap.tests "map" @*)
  (*TestAssoc.tests "assoc" @*)
  TestAssocAlt.tests "assocalt"
