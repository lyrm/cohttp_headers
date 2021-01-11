type t = (LString.t * string) list

let compare = Stdlib.compare

let headers_with_list_values =
  Array.map LString.of_string
    [|
      "accept";
      "accept-charset";
      "accept-encoding";
      "accept-language";
      "accept-ranges";
      "allow";
      "cache-control";
      "connection";
      "content-encoding";
      "content-language";
      "expect";
      "if-match";
      "if-none-match";
      "link";
      "pragma";
      "proxy-authenticate";
      "te";
      "trailer";
      "transfer-encoding";
      "upgrade";
      "vary";
      "via";
      "warning";
      "www-authenticate";
    |]

let is_transfer_encoding k' =
  let k = LString.of_string "transfer-encoding" in
  LString.compare k k' = 0

let is_header_with_list_value h =
  let tbl = Hashtbl.create (Array.length headers_with_list_values) in
  headers_with_list_values |> Array.iter (fun h -> Hashtbl.add tbl h ());
  Hashtbl.mem tbl h

let init () = []

let is_empty = function [] -> true | _ -> false

let init_with k v = [ (LString.of_string k, v) ]

let get h k =
  let k = LString.of_string k in
  let rec loop h =
    match h with
    | [] -> None
    | (k', v) :: h' -> if LString.compare k k' = 0 then Some v else loop h'
  in
  loop h

let add h k v =
  let k = LString.of_string k in
  let rec loop = function
    | [] -> [(k, v)]
    | (k', v') :: hs ->
        if LString.compare k k' = 0 then
          if is_header_with_list_value k then (k, v' ^ ", " ^ v) :: hs
          else (k, v) :: hs
        else (k', v') :: loop hs
  in
  loop h

let add_list h l = List.map (fun (k, v) -> (LString.of_string k, v)) l @ h

let todo () = failwith "todo"

let add_multi _h _k _vals = todo ()

let add_opt h_opt k v =
  match h_opt with None -> init_with k v | Some h -> add h k v

let add_unless_exists _h _k _v = todo ()

let add_opt_unless_exists _h _k _v = todo ()

let remove h k =
  let k = LString.of_string k in
  let rec loop seen = function
    | [] -> if seen then [] else raise Not_found
    | (k', _) :: h when LString.compare k' k = 0 -> loop true h
    | x :: h -> x :: loop seen h
  in
  try loop false h with Not_found -> h

let replace _h _k _v = todo ()

let update _h _k _f = todo ()

let mem _h _k = todo ()

let get_multi _h _k = todo ()

let iter _f _h = todo ()

let map _f _h = todo ()

let fold _f _h _init = todo ()

let of_list _h = todo ()

let to_list h =
  List.map (fun (k, v) -> LString.to_string k, v) h

let to_lines _h = todo ()

let to_frames _h = todo ()

let to_string _h = todo ()

let get_content_range _h = todo ()

let get_connection_close _h = todo ()

let user_agent = Printf.sprintf "ocaml-cohttp/%s" "http/1.1"

let prepend_user_agent headers user_agent =
  let k = "user-agent" in
  match get headers k with
  | Some ua -> replace headers k (user_agent ^ " " ^ ua)
  | None -> add headers k user_agent

let connection h =
  match get h "connection" with
  | Some v when v = "keep-alive" -> Some `Keep_alive
  | Some v when v = "close" -> Some `Close
  | Some x -> Some (`Unknown x)
  | _ -> None

(* RFC 2616 (https://tools.ietf.org/html/rfc2616#section-4.2)
   Multiple message-header fields with the same field-name MAY be
      present in a message if and only if the entire field-value for that
      header field is defined as a comma-separated list [i.e., #(values)].
      It MUST be possible to combine the multiple header fields into one
      "field-name: field-value" pair, without changing the semantics of the
      message, by appending each subsequent field-value to the first, each
      separated by a comma. The order in which header fields with the same
      field-name are received is therefore significant to the
      interpretation of the combined field value, and thus a proxy MUST NOT
      change the order of these field values when a message is forwarded.
 *)

(* https://tools.ietf.org/html/rfc7230#page-22

   In brief :
   single-value field :
    - don't care about order of fields with differing names
    - should appear 1 time max

   Field with multiple comma-separated values :
    - can appear multiple time
    - order must be kept
    - can be concataned

   Exception :
      Note: In practice, the "Set-Cookie" header field ([RFC6265]) often
      appears multiple times in a response message and does not use the
      list syntax, violating the above requirements on multiple header
      fields with the same name.  Since it cannot be combined into a
      single field-value, recipients ought to handle "Set-Cookie" as a
      special case while processing header fields.  (See Appendix A.2.3
      of [Kri2001] for details.)
*)
