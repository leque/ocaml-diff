module PTest = Ppx_test.Test

exception Unequal of string * string * string

let assert_equal ?(msg = "") ?printer ?(equal = (=)) expected actual =
  let print x = match printer with
  | Some p -> p x
  | None -> "<no printer>"
  in
  if not @@ equal expected actual then begin
    raise @@ Unequal (msg, print expected, print actual)
  end

let string_of_list cs =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

module StringDiff = Diff.Make(struct
  type t = String.t
  type elem = Char.t
  let get t i = String.get t i
  let length t = String.length t
end)

let ntest = 1000

let %TEST_UNIT "empty case" =
  assert_equal [] @@ StringDiff.lcs "" ""

let %TEST_UNIT "same input" =
  QCheck.Arbitrary.(generate ~n:ntest string)
  |> List.iter (fun s ->
      StringDiff.lcs s s
      |> List.map (function `Common (_, _, c) -> c)
      |> string_of_list
      |> assert_equal ~msg:s ~printer:(fun x -> x) s)

let %TEST_UNIT "Diff.fold_left: seq1 can be rebuild from `Common and `Removed" =
  let assert_equal x y =
    assert_equal
      ~msg:(Printf.sprintf "diff \"%s\" \"%s\""
              (String.escaped x) (String.escaped y))
      ~printer:(fun x -> x)
      x y
  in
  QCheck.Arbitrary.(generate ~n:ntest (pair string string))
  |> List.iter (fun (s, t) ->
      let v = StringDiff.fold_left ~init:[] s t ~f:(fun xs x ->
        match x with
        | `Common (_, _, c)
        | `Removed (_, c) -> c :: xs
        | `Added (_, _) -> xs)
        |> List.rev |> string_of_list
      in
      assert_equal s v)

let %TEST_UNIT "Diff.fold_left: seq2 can be rebuild from `Common and `Added" =
  let assert_equal x y =
    assert_equal
      ~msg:(Printf.sprintf "diff \"%s\" \"%s\""
              (String.escaped x) (String.escaped y))
      ~printer:(fun x -> x)
      x y
  in
  QCheck.Arbitrary.(generate ~n:ntest (pair string string))
  |> List.iter (fun (s, t) ->
      let v = StringDiff.fold_left ~init:[] s t ~f:(fun xs x ->
        match x with
        | `Common (_, _, c)
        | `Added (_, c) -> c :: xs
        | `Removed (_, _) -> xs)
        |> List.rev |> string_of_list
      in
      assert_equal t v)

let %TEST_UNIT "Diff.fold_left: positions are increased by one" =
  QCheck.Arbitrary.(generate ~n:ntest (pair string string))
  |> List.iter (fun (s, t) ->
      let assert_equal ~msg x y =
        assert_equal
          ~msg:(Format.sprintf "diff \"%s\" \"%s\": %s"
                  (String.escaped s) (String.escaped t) msg)
          ~printer:string_of_int
          x y
      in
      let (a, b) = StringDiff.fold_left ~init:(-1, -1) s t
          ~f:(fun (a, b) x ->
            match x with
            | `Common (x, y, _) ->
                assert_equal ~msg:"common x" (a + 1) x;
                assert_equal ~msg:"common y" (b + 1) y;
                (x, y)
            | `Removed (x, _) ->
                assert_equal ~msg:"removed" (a + 1) x;
                (x, b)
            | `Added (y, _) ->
                assert_equal ~msg:"added" (b + 1) y;
                (a, y))
      in
      assert_equal ~msg:"final a" (String.length s - 1) a;
      assert_equal ~msg:"final b" (String.length t - 1) b)

let %TEST_UNIT "Diff.iter: positions are increased by one" =
  QCheck.Arbitrary.(generate ~n:ntest (pair string string))
  |> List.iter (fun (s, t) ->
      let assert_equal ~msg x y =
        assert_equal
          ~msg:(Format.sprintf "diff \"%s\" \"%s\": %s"
                  (String.escaped s) (String.escaped t) msg)
          ~printer:string_of_int
          x y
      in
      let a = ref @@ -1 in
      let b = ref @@ -1 in
      StringDiff.iter s t
          ~f:(fun x ->
            match x with
            | `Common (x, y, _) ->
                assert_equal ~msg:"common x" (!a + 1) x;
                assert_equal ~msg:"common y" (!b + 1) y;
                a := x;
                b := y
            | `Removed (x, _) ->
                assert_equal ~msg:"removed" (!a + 1) x;
                a := x
            | `Added (y, _) ->
                assert_equal ~msg:"added" (!b + 1) y;
                b := y
             );
      assert_equal ~msg:"final a" (String.length s - 1) !a;
      assert_equal ~msg:"final b" (String.length t - 1) !b)

let diff expected actual =
  let buf = Buffer.create 16 in
  StringDiff.iter expected actual ~f:(function
    | `Common (_, _, c) ->
        Buffer.add_char buf c
    | `Removed (_, c) -> begin
        Buffer.add_string buf "\x1B[31m{-";
        Buffer.add_char buf c;
        Buffer.add_string buf "-}\x1B[0m";
    end
    | `Added (_, c) -> begin
        Buffer.add_string buf "\x1B[32m{+";
        Buffer.add_char buf c;
        Buffer.add_string buf "+}\x1B[0m";
    end);
  Buffer.contents buf

let () = PTest.collect ()
