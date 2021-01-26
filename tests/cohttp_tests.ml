module CohttpTests (H : Headers.HEADERS) = struct
  open Utils

  let t_header =
    Alcotest.testable
      (fun fmt h ->
        Fmt.pf fmt "%a"
          (Fmt.list ~sep:(Fmt.sps 1)
             (Fmt.pair ~sep:Fmt.comma Fmt.string Fmt.string))
          (H.to_list h))
      (fun x y -> H.compare x y = 0)

  (* Test that *)
  let list_valued_header () =
    let h = H.init () in
    let h = H.add h "accept" "foo" in
    let h = H.add h "accept" "bar" in
    aeso "list valued header" (H.get h "accept") (Some "bar,foo")

  let many_headers () =
    let size = 1000000 in
    let rec add_header num h =
      match num with
      | 0 -> h
      | n ->
          let k = Printf.sprintf "h%d" n in
          let v = Printf.sprintf "v%d" n in
          let h = H.add h k v in
          add_header (num - 1) h
    in
    let h = add_header size (H.init ()) in
    aei "many_headers" (List.length (H.to_list h)) size

  module Updates = struct
    let h =
      H.init () |> fun h ->
      H.add h "first" "1" |> fun h ->
      H.add h "second" "2" |> fun h ->
      H.add h "accept" "foo" |> fun h -> H.add h "accept" "bar"

    let replace_headers_if_exists () =
      let h = H.replace h "second" "2a" in
      aeso "replace_existing_header" (Some "2a") (H.get h "second")

    let replace_headers_if_absent () =
      let h = H.replace h "third" "3" in
      aeso "replace_new_header" (Some "3") (H.get h "third")

    let update_headers_if_exists () =
      let h1 =
        H.update h "second" (function Some _ -> Some "2a" | None -> None)
      in
      let h2 = H.replace h "second" "2a" in
      Alcotest.(check t_header) "update_existing_header" h1 h2

    let update_headers_if_exists_rm () =
      let h1 =
        H.update h "second" (function Some _ -> None | None -> Some "3")
      in
      let h2 = H.remove h "second" in
      Alcotest.(check t_header) "update_remove_header" h1 h2

    let update_headers_if_absent_add () =
      let h =
        H.update h "third" (function Some _ -> None | None -> Some "3")
      in
      aeso "update_add_new_header" (Some "3") (H.get h "third")

    let update_headers_if_absent_rm () =
      let h1 = H.update h "third" (function _ -> None) in
      Alcotest.(check t_header) "update_remove_absent_header" h h1

    let update_headers_if_exists_multi () =
      let h1 =
        H.update h "accept" (function
          | Some v -> Some ("baz," ^ v)
          | None -> None)
      in
      let h2 = H.add h "accept" "baz" in
      aeso "update_existing_header_multivalued" (H.get h1 "accept")
        (H.get h2 "accept")

    let update_headers_if_absent () =
      let h1 =
        H.update h "third" (function Some _ -> Some "3" | None -> None)
      in
      Alcotest.(check t_header) "update_new_header: unchanged" h h1;
      aeso "update_new_header: map unchanged" None (H.get h "third")
  end

  module Content_range = struct
    let h1 = H.of_list [ ("Content-Length", "123") ]

    let h2 = H.of_list [ ("Content-Range", "bytes 200-300/1000") ]

    let aeio = Alcotest.(check (option int64))

    let none () = aeio "none" None (H.init () |> H.get_content_range)

    let content_length () =
      aeio "content_length" (Some 123L) (H.get_content_range h1)

    let content_range () =
      aeio "content_range" (Some 101L) (H.get_content_range h2)
  end

  let transfer_encoding () =
    let h =
      H.of_list
        [ ("transfer-encoding", "gzip"); ("transfer-encoding", "chunked") ]
    in
    let sh = H.to_string h in
    aes "transfer_encoding_string_is_ordered" sh
      "transfer-encoding: gzip\r\ntransfer-encoding: chunked\r\n\r\n";
    let sh = H.get h "transfer-encoding" in
    aeso "transfer_encoding_get_is_ordered" (Some "gzip,chunked") sh

  let tests name =
    [
      ( name ^ " - Cohttp-Content Range",
        [
          ("none", `Quick, Content_range.none);
          ("content-length", `Quick, Content_range.content_length);
          ("content-range", `Quick, Content_range.content_range);
        ] );
      ( name ^ " - Cohttp-Header",
        [
          ("get list valued", `Quick, list_valued_header);
          ("replace existing", `Quick, Updates.replace_headers_if_exists);
          ("replace absent", `Quick, Updates.replace_headers_if_absent);
          ("update existing", `Quick, Updates.update_headers_if_exists);
          ( "update existing list",
            `Quick,
            Updates.update_headers_if_exists_multi );
          ("update add absent", `Quick, Updates.update_headers_if_absent_add);
          ("update rm existing", `Quick, Updates.update_headers_if_exists_rm);
          ("update rm absent", `Quick, Updates.update_headers_if_absent_rm);
          ("update absent", `Quick, Updates.update_headers_if_absent);
          ("many headers", `Slow, many_headers);
          ("transfer encoding is in correct order", `Quick, transfer_encoding);
        ] );
    ]
end

module TestMap = CohttpTests (Headers.HeadersMap)
module TestAssoc = CohttpTests (Headers.HeadersAssoc)
module TestAssocAlt = CohttpTests (Headers.HeadersAssocAlt)

let tests =
  (*TestMap.tests "map" @*) (*TestAssoc.tests "assoc" @*) TestAssocAlt.tests "assocalt"
