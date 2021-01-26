let aes = Alcotest.check Alcotest.string

let aeso = Alcotest.check Alcotest.(option string)

let aesl = Alcotest.check Alcotest.(list string)

let aessl = Alcotest.check Alcotest.(list (pair string string))

let aeb = Alcotest.check Alcotest.bool

let aei = Alcotest.check Alcotest.int
