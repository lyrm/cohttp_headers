type t = (LString.t * string) list

let compare = Stdlib.compare

let user_agent = Printf.sprintf "ocaml-cohttp/%s" "http/1.1"

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

let add h k v =
  let k = LString.of_string k in
  let to_add = ref false in
  let rec loop = function
    | [] ->
        to_add := true;
        []
    | (k', v') :: hs ->
        if LString.compare k k' = 0 then
          if is_header_with_list_value k then (k, v' ^ ", " ^ v) :: hs
          else (
            to_add := true;
            hs )
        else (k', v') :: loop hs
  in
  let h = loop h in
  if !to_add then (k, v) :: h else h

let get h k =
  let k = LString.of_string k in
  let rec loop h =
    match h with
    | [] -> None
    | (k', v) :: h' -> if LString.compare k k' = 0 then Some v else loop h'
  in
  loop h

let mem h k =
  let k = LString.of_string k in
  let rec loop = function
    | [] -> false
    | (k', _) :: h' -> if LString.compare k k' = 0 then true else loop h'
  in
  loop h

let add_list h l = List.fold_left (fun h' (k, v) -> add h' k v) h (List.rev l)

(* reverse order, whereas add_list keep order*)
let add_multi h k vals = List.fold_left (fun acc v -> add acc k v) h vals

let add_opt h_opt k v =
  match h_opt with None -> init_with k v | Some h -> add h k v

let add_unless_exists h k v = if mem h k then add h k v else h

let add_opt_unless_exists h k v =
  match h with None -> init_with k v | Some h -> add_unless_exists h k v

(* A key should appear only once *)
let remove h k =
  let k = LString.of_string k in
  let rec loop = function
    | [] -> []
    | (k', v) :: h' ->
        if LString.compare k k' = 0 then h' else (k', v) :: loop h'
  in
  loop h

let replace h k v =
  let k = LString.of_string k in
  let rec loop = function
    | [] -> [ (k, v) ]
    | (k', v') :: h' ->
        if LString.compare k k' = 0 then (k, v) :: h' else (k', v') :: loop h'
  in
  loop h

let update h k f =
  let vorig = get h k in
  match (f vorig, vorig) with
  | None, _ -> remove h k
  | Some s, Some s' when s == s' -> h
  | Some s, _ -> replace h k s

(* Should return the same result than get *)
let get_multi h k =
  match get h k with
  | None -> []
  | Some vals -> String.split_on_char ',' vals |> List.map String.trim

let map (f : string -> string -> string) (h : t) : t =
  List.map
    (fun (k, v) ->
      let v' = f (LString.to_string k) v in
      (k, v'))
    h

let iter (f : string -> string -> unit) (h : t) : unit =
  List.iter (fun (k, v) -> f (LString.to_string k) v) h

let fold (f : string -> string -> 'a -> 'a) (h : t) (init : 'a) : 'a =
  List.fold_left (fun acc (k, v) -> f (LString.to_string k) v acc) init h

(* really *)
let of_list l = List.fold_right (fun (k, v) h -> add h k v) l (init ())

let to_list h = List.map (fun (k, v) -> (LString.to_string k, v)) h

let to_lines (h : t) : string list =
  let header_line k v = Printf.sprintf "%s: %s\r\n" k v in
  List.map (fun (k, v) -> header_line (LString.to_string k) v) h

let to_frames h =
  let to_frame k v = Printf.sprintf "%s: %s" k v in
  List.map (fun (k, v) -> to_frame (LString.to_string k) v) h

let to_string h =
  let b = Buffer.create 128 in
  h
  |> iter (fun k v ->
         Buffer.add_string b k;
         Buffer.add_string b ": ";
         Buffer.add_string b v;
         Buffer.add_string b "\r\n");
  Buffer.add_string b "\r\n";
  Buffer.contents b

let parse_content_range s =
  try
    let start, fini, total =
      Scanf.sscanf s "bytes %Ld-%Ld/%Ld" (fun start fini total ->
          (start, fini, total))
    in
    Some (start, fini, total)
  with Scanf.Scan_failure _ -> None

(* If we see a "Content-Range" header, than we should limit the
   number of bytes we attempt to read *)
let get_content_range headers =
  match get headers "content-length" with
  | Some clen -> ( try Some (Int64.of_string clen) with _ -> None )
  | None -> (
      match get headers "content-range" with
      | Some range_s -> (
          match parse_content_range range_s with
          | Some (start, fini, total) ->
              (* some sanity checking before we act on these values *)
              if fini < total && start <= total && 0L <= start && 0L <= total
              then
                let num_bytes_to_read = Int64.add (Int64.sub fini start) 1L in
                Some num_bytes_to_read
              else None
          | None -> None )
      | None -> None )

let get_connection_close headers =
  match get headers "connection" with Some "close" -> true | _ -> false

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
