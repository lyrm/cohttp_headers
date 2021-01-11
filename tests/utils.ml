module H = Headers.HeadersMap
module Ha = Headers.HeadersAssoc

let aes = Alcotest.check Alcotest.string
let aeso = Alcotest.check Alcotest.(option string)

let t_header =
  Alcotest.testable (fun fmt h ->
      Fmt.pf fmt "%a"
        (Fmt.list ~sep:(Fmt.sps 1) (Fmt.pair ~sep:Fmt.comma Fmt.string Fmt.string ) )
        (H.to_list h)
    ) (fun x y -> H.compare x y = 0)
