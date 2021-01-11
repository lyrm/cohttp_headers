open Bechamel

(*open Toolkit*)

let build name =
  List.map (fun (n, f) -> Test.make ~name:(name ^ "/" ^ n) (Staged.stage f))

let display instance results =
  let res =
    let open Bechamel_js in
    emit ~dst:(Channel stdout)
      (fun _ -> Ok ())
      ~x_label:Measure.run ~y_label:(Measure.label instance) results
  in
  (match res with Ok () -> () | Error (`Msg err) -> invalid_arg err);
  if Sys.command "" = 1 then () else failwith "plop"

let benchmark tested instances =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let cfg =
    Bechamel.Benchmark.cfg ~start:1 ~sampling:(`Linear 10) ~limit:200
      ~quota:(Time.second 30.) ~kde:(Some 200) ()
  in
  let raw_results = Bechamel.Benchmark.all cfg instances tested in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let main _all =
  let monotonic_clock = Toolkit.Instance.monotonic_clock in
  let tested =
    List.fold_left
      (fun t Benchmarked_functions.{ name; fs } -> build name fs @ t)
      [] Benchmarked_functions.tests
  in
  let tested = Test.make_grouped ~name:"" ~fmt:"%s%s" tested in
  (* Benchmark analysed result and raw measures.*)
  let results = benchmark tested [ monotonic_clock ] in
  (* Create the json output *)
  let res =
    let open Bechamel_js in
    emit ~dst:(Channel stdout)
      (fun _ -> Ok ())
      ~x_label:Measure.run
      ~y_label:(Measure.label monotonic_clock)
      results
  in
  match res with Ok () -> () | Error (`Msg err) -> invalid_arg err
