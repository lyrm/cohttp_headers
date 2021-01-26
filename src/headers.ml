module type HEADERS = sig
  type t

  val init : unit -> t
  (** Construct a fresh, empty map of HTTP headers. *)

  val is_empty : t -> bool
  (** Test whether HTTP headers are empty or not. *)

  val init_with : string -> string -> t
  (** Construct a fresh map of HTTP headers with a single key and value entry. *)

  val add : t -> string -> string -> t
  (** Add a key and value to an existing header map. *)

  val add_list : t -> (string * string) list -> t
  (** Add multiple key and value pairs to an existing header map. *)

  val add_multi : t -> string -> string list -> t
  (** Add multiple values to a key in an existing header map. *)

  val add_opt : t option -> string -> string -> t
  (** Given an optional header, either update the existing one with
      a key and value, or construct a fresh header with those values if
      the header is [None]. *)

  val add_unless_exists : t -> string -> string -> t
  (** Given a header, update it with the key and value unless the key is
      already present in the header. *)

  val add_opt_unless_exists : t option -> string -> string -> t
  (** [add_opt_unless_exists h k v] updates [h] with the key [k] and value [v]
      unless the key is already present in the header.  If [h] is [None]
      then a fresh header is allocated containing the key [k] and the
      value [v]. *)

  val remove : t -> string -> t
  (** Remove a key from the header map and return a fresh header set. The
      original header parameter is not modified. *)

  val replace : t -> string -> string -> t
  (** Replace the value of a key from the header map if it exists, otherwise it
      adds it to the header map. The original header parameter is not modified. *)

  val update : t -> string -> (string option -> string option) -> t
  (** [update h k f] returns a map containing the same headers as [h],
      except for the header [k]. Depending on the value of [v] where [v] is
      [f (get h k)], the header [k] is added, removed or updated.
      If [v] is [None], the header is removed if it exists; otherwise,
      if [v] is [Some z] then [k] is associated to [z] in the resulting headers.
      If [k] was already associated in [h] to a value that is physically equal
      to [z], [h] is returned unchanged. Similarly as for [get], if the header is
      one of the set of headers defined to have list values, then all of the values
      are concatenated into a single string separated by commas and passed to [f],
      while the return value of [f] is split on commas and associated to [k].
      If it is a singleton header, then the first value is passed to [f] and
      no concatenation is performed, similarly for the return value.
      The original header parameters are not modified. *)

  val mem : t -> string -> bool
  (** Check if a key exists in the header. *)

  val compare : t -> t -> int
  (** Structural comparison of two [Header] values. *)

  val get : t -> string -> string option
  (** Retrieve a key from a header.  If the header is one of the set of
    headers defined to have list values, then all of the values are
    concatenated into a single string separated by commas and returned.
    If it is a singleton header, then the first value is selected and
    no concatenation is performed. *)

  val get_multi : t -> string -> string list
  (** Retrieve all of the values associated with a key *)

  val iter : (string -> string -> unit) -> t -> unit

  val map : (string -> string -> string) -> t -> t

  val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

  val of_list : (string * string) list -> t

  val to_list : t -> (string * string) list

  val to_lines : t -> string list
  (** Return header fieds as a list of lines. Beware that each line
      ends with "\r\n" characters. *)

  val to_frames : t -> string list
  (** Same as {!to_lines} but lines do not end with "\r\n" characters. *)

  val to_string : t -> string

  val get_content_range : t -> Int64.t option

  val get_connection_close : t -> bool

  val user_agent : string
  (** The User-Agent header used by this library, including the version
    of cohttp. *)

  val prepend_user_agent : t -> string -> t
  (** Prepend [user_agent] to the product token already declared in the
    "User-Agent" field (if any). *)

  val connection : t -> [ `Keep_alive | `Close | `Unknown of string ] option
end

module HeadersMap : HEADERS = Headers_map

module HeadersAssoc : HEADERS = Headers_assoc

module HeadersAssocAlt : HEADERS = Headers_assoc2
