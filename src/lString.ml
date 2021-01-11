type t = string

let of_string x = String.lowercase_ascii x

let to_string x = x

let compare a b = String.compare a b
