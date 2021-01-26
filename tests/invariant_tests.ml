module Invariants (H : Headers.HEADERS) = struct
  open Utils

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

  let header = H.of_list hstr

  (* Suite test for Header module *)
  (* Tested invariants:
     - mem (init ()) k = false
     - mem (add (init ()) k v) k = true
  *)
  let mem_tests () =
    aeb "Header.mem (Header.init ()) k = false" false H.(mem (init ()) "accept");
    aeb "Header.mem (Header.add (Header.init ()) k v) k = true" true
      H.(mem (add (init ()) "accept" "text/*") "accept")

  (* Tested invariants:
     - get (add h k v) k = Some v with mem h k = false
     - get (add h k v) k = Some v with mem h k = true
  *)
  (* Note: For multiple-value headers, the previous Header module would have
     returns to get(add h k v) "[previous_value], v" *)
  let add_get_tests () =
    aeso "Header.get (Header.add h k v) k = Some v with Header.mem h k = false"
      (Some "keep-alive")
      H.(get (add header "connection" "keep-alive") "connection");
    aeso "Header.get (Header.add h k v) k = Some v with Header.mem h k = true"
      (Some "text/*")
      H.(get (add header "accept" "text/*") "accept")

  (* Tested invariants:
     - get (add_unless_exists h k v) k = v if mem h k = true
     - get (add_unless_exists h k v) k = v' if get h k = v'
  *)
  let add_unless_exists_get_tests () =
    aeso
      "Header.get (Header.add_unless_exists h k v) k = v if Header.mem h k = \
       true"
      (Some "close")
      H.(get (add_unless_exists header "connection" "close") "connection");
    aeso
      "Header.get (Header.add_unless_exists h k v) k = v' if Header.get h k = \
       v'"
      (Some "chunked")
      H.(
        get
          (add_unless_exists header "transfer-encoding" "gzip")
          "transfer-encoding")

  (* Tested invariants:
     - get_multi (add h k n) k = [n] if mem h k = None
     - get_multi (add h k n) k = vs@[n] if get_multi h k = vs
  *)
  let add_get_multi_tests () =
    aesl "Header.get_multi (Header.add h k n) k = [n] if Header.mem h k = None"
      [ "keep-alive" ]
      H.(get_multi (add header "connection" "keep-alive") "connection");
    aesl
      "Header.get_multi (Header.add h k n) k = vs@[n] if Header.get_multi h k \
       = vs"
      [ "application/xml"; "text/html"; "image/*" ]
      H.(get_multi (add header "accept" "image/*") "accept")

  (* Tested invariants:
     - get_multi (add_multi h k []) k = vs with get_multi h k = vs
     - get_multi (add_multi h k vs) k = vs with mem h k = false
     - get_multi (add_multi h k vs) k = vs'@vs with get_multi h k = vs'
  *)
  let add_multi_get_multi_tests () =
    aesl
      "Header.get_multi (Header.add_multi h k []) k' = vs with \
       Header.get_multi h k = vs"
      [ "application/xml"; "text/html" ]
      H.(get_multi (add_multi header "a" []) "accept");
    aesl
      "Header.get_multi (Header.add_multi h k vs) k = vs with Header.mem h k = \
       false"
      [ "keep-alive"; "close" ]
      H.(
        get_multi
          (add_multi header "connection" [ "keep-alive"; "close" ])
          "connection");
    aesl
      "Header.get_multi (Header.add_multi h k vs) k = vs'@vs with \
       Header.get_multi h k = vs' "
      [ "application/xml"; "text/html"; "image/*"; "application/xhtml" ]
      H.(
        get_multi
          (add_multi header "accept" [ "image/*"; "application/xhtml" ])
          "accept")

  (* Tested invariants
     - get_multi (add_list h [k, v; k', v']) k'' = [] if mem h k'' = false
     - get_multi (add_list h [k, v; k', v']) k = vs@[v] if get_multi h k = vs
     - get_multi (add_list h [k, v; k, v']) k = vs@[v;v'] if get_multi h k = vs
  *)
  let add_list_get_multi_tests () =
    aesl
      "Header.get_multi (Header.add_list h [k, v; k', v']) k'' = [] if \
       Header.mem h k'' = false"
      []
      H.(
        get_multi
          (add_list (init ())
             [ ("transfer-encoding", "chunked"); ("connection", "close") ])
          "accept");
    aesl
      "Header.get_multi (Header.add_list h [k, v; k', v']) k = vs@[v] if \
       Header.get_multi h k = vs "
      [ "application/xml"; "text/html"; "image/*" ]
      H.(
        get_multi
          (add_list header [ ("accept", "image/*"); ("connection", "close") ])
          "accept");
    aesl
      "Header.get_multi (Header.add_list h [k, v; k, v']) k = vs@[v;v'] if \
       Header.get_multi h k = vs "
      [ "application/xml"; "text/html"; "image/*"; "application/*" ]
      H.(
        get_multi
          (add_list header
             [ ("accept", "image/*"); ("accept", "application/*") ])
          "accept")

  (* Tested invariants
     - to_list (init ())
     - to_list (of_list h) has the same elements than h
  *)
  (* The overall order is not the same since cohttp used a map structure
     for headers where http/af use an associative list.
  *)
  let to_list_tests () =
    Alcotest.(check (list (pair string string)))
      "Header.to_list (Header.init ())" []
      H.(to_list (init ()));
    aeb "Header.to_list (Header.of_list h) has the same elements than h." true
      (List.fold_left
         (fun acc elt -> List.mem elt hstr && acc)
         true (H.to_list header))

  (* Tested invariants:
     - mem (remove h n) n = false if get_multi h n = []
     - mem (remove h n) n = false if get_multi h n = [v]
     - mem (remove h n) n = false if get_multi h n = vs
     - mem (remove h n) n' = true if mem h n' = true
  *)
  let remove_mem_tests () =
    aeb "Header.mem (Header.remove h n) n = false if Header.get_multi h n = []"
      false
      H.(mem (remove header "connection") "connection");
    aeb "Header.mem (Header.remove h n) n = false if Header.get_multi h n = [v]"
      false
      H.(mem (remove header "transfer-encoding") "transfer-encoding");
    aeb "Header.mem (Header.remove h n) n = false if Header.get_multi h n = vs"
      false
      H.(mem (remove header "accept") "accept");
    aeb "Header.mem (Header.remove h n) n' = true if Header.mem h n' = true"
      true
      H.(mem (remove header "accept") "transfer-encoding")

  (* Tested invariants:
     - get_multi (replace h k v) k = [] if [get_multi h v] = []
     - get_multi (replace h k v) k = [v] if [get_multi h v] = _ :: _
  *)
  let replace_tests () =
    aesl
      "Header.get_multi (Header.replace h k v) k = [] if Header.mem h k = false"
      []
      H.(get_multi (replace header "connection" "close") "connection");
    aesl
      "Header.get_multi (Header.replace h k v) k = v if Header.mem h k = true"
      [ "gzip" ]
      H.(
        get_multi
          (replace header "transfer-encoding" "gzip")
          "transfer-encoding")

  (* Tested invariants
     - compare (to_list l) (add_list (init ()) l) = 0

     TODO : Add more tests for this function !
  *)
  let compare_tests () =
    aei
      "Header.compare (Header.to_list l) (Header.add_list (Header.init ()) l) \
       = 0"
      0
      H.(compare (of_list hstr) (add_list (init ()) hstr))

  (*
   TODO :
  val of_list : (name * value) list -> t
  val fold : (name -> value -> 'a -> 'a) -> 'a -> t -> 'a
  val to_string : t -> string

  (* Incompatible definitions :
        cohttp uses a function of type : name -> value list -> unit
        httpaf uses a function of type : name -> value -> unit
     val iter : (name -> value -> unit) -> t -> unit*)
*)

  let tests name =
    [
      ( name,
        [
          ("Header.mem", `Quick, mem_tests);
          ("Header.add and Header.get", `Quick, add_get_tests);
          ( "Header.add_unless_exists and Header.get",
            `Quick,
            add_unless_exists_get_tests );
          ("Header.add and Header.get_multi", `Quick, add_get_multi_tests);
          ( "Header.add_multi and Header.get_multi",
            `Quick,
            add_multi_get_multi_tests );
          ( "Header.add_list and Header.get_multi",
            `Quick,
            add_list_get_multi_tests );
          ("Header.to_list", `Quick, to_list_tests);
          ("Header.remove", `Quick, remove_mem_tests);
          ("Header.replace", `Quick, replace_tests);
          ("Header.compare and various invariants", `Quick, compare_tests);
        ] );
    ]
end

module TestMap = Invariants (Headers.HeadersMap)
module TestAssoc = Invariants (Headers.HeadersAssoc)
module TestAssocAlt = Invariants (Headers.HeadersAssocAlt)

let tests =
  (*TestMap.tests "map" @*)
  (*TestAssoc.tests "assoc" @*)
  TestAssocAlt.tests "assocalt"
