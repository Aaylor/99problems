
open OUnit
open Problems

let test_problem_001 =
  let open Problem001 in
  let tests = [
    "last empty list" >:: (fun _ ->
        assert_equal (last []) None
      );

    "last singleton list" >:: (fun _ ->
        assert_equal (last [42]) (Some 42)
      );

    "last list" >:: (fun _ ->
        assert_equal (last [39; 40; 41; 42]) (Some 42)
      )
  ]
  in
  if tests = [] then None else Some ("Problem001" >::: tests)


let test_problem_002 =
  let open Problem002 in
  let tests = [
    "last_two empty list" >:: (fun _ ->
        assert_equal (last_two []) None
      );

    "last_two singleton list" >:: (fun _ ->
        assert_equal (last_two [1]) None
      );

    "last_two two elements list" >:: (fun _ ->
        assert_equal (last_two [1; 2]) (Some (1, 2))
      );

    "last_two n elements list (n > 2)" >:: (fun _ ->
        assert_equal (last_two [1; 2; 3; 4; 5; 6; 41; 42]) (Some (41, 42))
      );
  ]
  in
  if tests = [] then None else Some ("Problem002" >::: tests)


let test_problem_003 =
  let open Problem003 in
  let tests = [
    "at empty list" >:: (fun _ ->
        assert_equal (at 42 []) None
      );

    "at non empty list fail" >:: (fun _ ->
        assert_equal (at 42 [1; 2; 3]) None
      );

    "at non empty list success" >:: (fun _ ->
        assert_equal (at 2 [1; 2; 3]) (Some 2)
      )
  ]
  in
  if tests = [] then None else Some ("Problem003" >::: tests)


let test_problem_004 =
  let open Problem004 in
  let tests = [
    "length empty list" >:: (fun _ ->
        assert_equal (length []) 0
      );

    "length non empty list" >:: (fun _ ->
        assert_equal (length [1; 2; 3; 4; 5]) 5
      )
  ]
  in
  if tests = [] then None else Some ("Problem004" >::: tests)


let test_problem_005 =
  let open Problem005 in
  let tests = [
    "rev empty list" >:: (fun _ ->
        assert_equal (rev []) []
      );

    "rev non empty list" >:: (fun _ ->
        assert_equal (rev [1; 2; 3; 4]) [4; 3; 2; 1]
      )
  ]
  in
  if tests = [] then None else Some ("Problem005" >::: tests)


let test_problem_006 =
  let open Problem006 in
  let tests = [
    "palindrome empty list" >:: (fun _ ->
        assert_bool "should be true" (palindrome [])
      );

    "palindrome singleton list" >:: (fun _ ->
        assert_bool "should be true" (palindrome [])
      );

    "palindrome success" >:: (fun _ ->
        assert_bool "should be true" (palindrome [1; 2; 3; 2; 1])
      );

    "palindrome failure" >:: (fun _ ->
        assert_bool "should be false" (not (palindrome [1; 2; 3; 2; 4]))
      )
  ]
  in
  if tests = [] then None else Some ("Problem006" >::: tests)


let test_problem_007 =
  let open Problem007 in
  let tests = [
    "flatten empty list" >:: (fun _ ->
        assert_equal (flatten []) []
      );

    "flatten singleton one" >:: (fun _ ->
        assert_equal (flatten [One 1]) [1]
      );

    "flatten singleton many" >:: (fun _ ->
        assert_equal (flatten [Many [One 1]]) [1]
      );

    "flatten" >:: (fun _ ->
        let t = [One 1; Many [One 2; Many [One 3; One 4 ]; One 5]] in
        assert_equal (flatten t) [1; 2; 3; 4; 5]
      )
  ]
  in
  if tests = [] then None else Some ("Problem007" >::: tests)


let test_problem_008 =
  let open Problem008 in
  let tests = [
    "compress empty" >:: (fun _ ->
        assert_equal (compress []) []
      );

    "compress list" >:: (fun _ ->
        let t = [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5] in
        assert_equal (compress t) [1; 2; 3; 1; 4; 5]
      )
  ]
  in
  if tests = [] then None else Some ("Problem008" >::: tests)


let test_problem_009 =
  let open Problem009 in
  let tests = [
    "pack empty" >:: (fun _ ->
        assert_equal (pack []) []
      );

    "pack list" >:: (fun _ ->
        let t = [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5] in
        assert_equal (pack t)
          [[1; 1; 1; 1]; [2]; [3; 3]; [1; 1]; [4]; [5; 5; 5; 5]]
      )
  ]
  in
  if tests = [] then None else Some ("Problem009" >::: tests)


let test_problem_010 =
  let open Problem010 in
  let tests = [
    "encode empty" >:: (fun _ ->
        assert_equal (encode []) []
      );

    "encode list" >:: (fun _ ->
        let t = [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5] in
        assert_equal (encode t)
          [(4, 1); (1, 2); (2, 3); (2, 1); (1, 4); (4, 5)]
      )
  ]
  in
  if tests = [] then None else Some ("Problem010" >::: tests)


let test_problem_011 =
  let open Problem011 in
  let tests = [
    "encode empty" >:: (fun _ ->
        assert_equal (encode []) []
      );

    "encode list" >:: (fun _ ->
        let t = [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5] in
        assert_equal (encode t)
          [Many (4, 1); One 2; Many (2, 3); Many (2, 1); One 4; Many (4, 5)]
      )
  ]
  in
  if tests = [] then None else Some ("Problem011" >::: tests)


let test_problem_012 =
  let open Problem012 in
  let tests = [
    "decode empty" >:: (fun _ ->
        assert_equal (decode []) []
      );

    "decode list" >:: (fun _ ->
        let t =
          [Many (4, 1); One 2; Many (2, 3); Many (2, 1); One 4; Many (4, 5)]
        in
        assert_equal (decode t) [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5]
      )
  ]
  in
  if tests = [] then None else Some ("Problem012" >::: tests)


let test_problem_013 =
  let open Problem013 in
  let tests = [
    "encode empty" >:: (fun _ ->
        assert_equal (encode []) []
      );

    "encode list" >:: (fun _ ->
        let t = [1; 1; 1; 1; 2; 3; 3; 1; 1; 4; 5; 5; 5; 5] in
        assert_equal (encode t)
          [Many (4, 1); One 2; Many (2, 3); Many (2, 1); One 4; Many (4, 5)]
      )
  ]
  in
  if tests = [] then None else Some ("Problem013" >::: tests)


let test_problem_014 =
  let open Problem014 in
  let tests = [
    "duplicate empty" >:: (fun _ ->
        assert_equal (duplicate []) []
      );

    "duplicate list" >:: (fun _ ->
        assert_equal (duplicate [1; 2; 3; 4]) [1; 1; 2; 2; 3; 3; 4; 4]
      )
  ]
  in
  if tests = [] then None else Some ("Problem014" >::: tests)


let test_problem_015 =
  let open Problem015 in
  let tests = [
    "replicate empty" >:: (fun _ ->
        assert_equal (replicate [] 42) []
      );

    "replicate list" >:: (fun _ ->
        assert_equal (replicate [1; 2; 3] 4)
          [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3]
      )
  ]
  in
  if tests = [] then None else Some ("Problem015" >::: tests)


let test_problem_016 =
  let open Problem016 in
  let tests = [
    "drop empty" >:: (fun _ ->
        assert_equal (drop [] 42) []
      );

    "drop list" >:: (fun _ ->
        assert_equal (drop [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 3)
          [1; 2; 4; 5; 7; 8; 10]
      )
  ]
  in
  if tests = [] then None else Some ("Problem016" >::: tests)


let test_problem_017 =
  let open Problem017 in
  let tests = [
    "drop empty" >:: (fun _ ->
        assert_equal (split [] 42) ([], [])
      );

    "drop > list length" >:: (fun _ ->
        assert_equal (split [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 11)
          ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10], [])
      );

    "drop list" >:: (fun _ ->
        assert_equal (split [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 4)
          ([1; 2; 3; 4], [5; 6; 7; 8; 9; 10])
      );

    "drop head empty" >:: (fun _ ->
        assert_equal (split [1; 2; 3; 4] 0) ([], [1; 2; 3; 4])  
      );
  ]
  in
  if tests = [] then None else Some ("Problem017" >::: tests)


let test_problem_018 =
  let open Problem018 in
  let tests = [
    "slice empty" >:: (fun _ ->
        assert_equal (slice [] 0 0) []
      );

    "slice list" >:: (fun _ ->
        assert_equal (slice [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 2 6)
          [3; 4; 5; 6; 7]
      );
  ]
  in
  if tests = [] then None else Some ("Problem018" >::: tests)


let test_problem_019 =
  let open Problem019 in
  let tests = [
    "rotate empty" >:: (fun _ ->
        assert_equal (rotate [] 42) []
      );

    "rotate list 1" >:: (fun _ ->
        assert_equal (rotate [1; 2; 3; 4; 5; 6; 7; 8] 3)
          [4; 5; 6; 7; 8; 1; 2; 3]
      );

    "rotate list 2" >:: (fun _ ->
        assert_equal (rotate [1; 2; 3; 4; 5; 6; 7; 8] (-2))
          [7; 8; 1; 2; 3; 4; 5; 6]
      )
  ]
  in
  if tests = [] then None else Some ("Problem019" >::: tests)


let test_problem_020 =
  let open Problem020 in
  let tests = [
    "remove_at" >:: (fun _ ->
        assert_equal (remove_at 1 [1; 2; 3; 4]) [1; 3; 4]
      )
  ]
  in
  if tests = [] then None else Some ("Problem020" >::: tests)


let test_problem_021 =
  let open Problem021 in
  let tests = [
    "insert_at" >:: (fun _ ->
        assert_equal (insert_at 42 2 [1; 2; 3; 4]) [1; 2; 42; 3; 4]
      );

    "insert_at 0" >:: (fun _ ->
        assert_equal (insert_at 42 0 [1; 2; 3; 4]) [42; 1; 2; 3; 4]
      )
  ]
  in
  if tests = [] then None else Some ("Problem021" >::: tests)


let test_problem_022 =
  let open Problem022 in
  let tests = [
    "range incr" >:: (fun _ ->
        assert_equal (range (-3) 3) [-3; -2; -1; 0; 1; 2; 3]
      );

    "range decr" >:: (fun _ ->
        assert_equal (range 3 (-3)) [3; 2; 1; 0; -1; -2; -3]
      )
  ]
  in
  if tests = [] then None else Some ("Problem022" >::: tests)


let test_problem_023 =
  let open Problem023 in
  let tests = [
    "rand_select" >:: (fun _ ->
        let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
        assert_bool "should be true"
          List.(for_all (fun x -> exists (( = ) x) l) (rand_select l 3))
      );
  ]
  in
  if tests = [] then None else Some ("Problem023" >::: tests)


let test_problem_024 =
  let open Problem024 in
  let tests = [
    "lotto" >:: (fun _ ->
        let nb  = 6  in
        let max = 49 in
        let lotto = lotto_select nb max in

        assert_equal nb (List.length lotto);
        assert_bool "should be true"
          (List.for_all (fun x -> x >= 1 && x <= 49) lotto);
      );
  ]
  in
  if tests = [] then None else Some ("Problem024" >::: tests)


let test_problem_025 =
  let open Problem025 in
  let tests = [
    "permutation" >:: (fun _ ->
        let l = [1; 2; 3] in
        let p = permutation l in

        assert_bool "should be true"
          (p = [1; 2; 3] || p = [1; 3; 2] || p = [2; 1; 3] ||
           p = [2; 3; 1] || p = [3; 1; 2] || p =[3; 2; 1])
      )
  ]
  in
  if tests = [] then None else Some ("Problem025" >::: tests)


let test_problem_026 =
  let open Problem026 in
  let tests = [
    "extract empty list" >:: (fun _ ->
        assert_equal (extract 42 []) []
      );

    "extract correct" >:: (fun _ ->
        assert_equal (extract 2 [1; 2; 3; 4])
          [[3; 4]; [2; 4]; [2; 3]; [1; 4]; [1; 3]; [1; 2]]
      );

    "extract one" >:: (fun _ ->
        assert_equal (extract 1 [1; 2; 3; 4]) [[4]; [3]; [2]; [1]]
      );

    "extract exact length" >:: (fun _ ->
        assert_equal (extract 4 [1; 2; 3; 4]) [[1; 2; 3; 4]]
      );

    "extract > length" >:: (fun _ ->
        assert_equal (extract 42 [1; 2; 3; 4]) []
      )
  ]
  in
  if tests = [] then None else Some ("Problem026" >::: tests)


let test_problem_027 =
  let open Problem027 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem027" >::: tests)


let test_problem_028 =
  let open Problem028 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem028" >::: tests)


let test_problem_029 =
  let open Problem029 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem029" >::: tests)


let test_problem_030 =
  let open Problem030 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem030" >::: tests)


let test_problem_031 =
  let open Problem031 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem031" >::: tests)


let test_problem_032 =
  let open Problem032 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem032" >::: tests)


let test_problem_033 =
  let open Problem033 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem033" >::: tests)


let test_problem_034 =
  let open Problem034 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem034" >::: tests)


let test_problem_035 =
  let open Problem035 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem035" >::: tests)


let test_problem_036 =
  let open Problem036 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem036" >::: tests)


let test_problem_037 =
  let open Problem037 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem037" >::: tests)


let test_problem_038 =
  let open Problem038 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem038" >::: tests)


let test_problem_039 =
  let open Problem039 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem039" >::: tests)


let test_problem_040 =
  let open Problem040 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem040" >::: tests)


let test_problem_041 =
  let open Problem041 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem041" >::: tests)


let test_problem_042 =
  let open Problem042 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem042" >::: tests)


let test_problem_043 =
  let open Problem043 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem043" >::: tests)


let test_problem_044 =
  let open Problem044 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem044" >::: tests)


let test_problem_045 =
  let open Problem045 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem045" >::: tests)


let test_problem_046 =
  let open Problem046 in
  let res =
    List.map2 (fun (x, y) z -> (x, y, z))
      [(false, false); (false, true); (true, false); (true, true)]
  in
  let a = "a" in
  let b = "b" in
  let tests = [
    "table2 var" >:: (fun _ ->
        assert_equal (table2 a b (Var a))
          (res [false; false; true; true])
      );

    "table2 var fail" >:: (fun _ ->
        assert_raises (Unknown_var "c") (fun _ ->
            ignore (table2 a b (Var "c"))
          )
      );

    "table2 not" >:: (fun _ ->
        assert_equal (table2 a b (Not (Var b)))
          (res [true; false; true; false])
      );

    "table2 and" >:: (fun _ ->
        assert_equal (table2 a b (And (Var a, Var b)))
          (res [false; false; false; true])
      );

    "table2 or" >:: (fun _ ->
        assert_equal (table2 a b (Or (Var a, Var b)))
          (res [false; true; true; true])
      )
  ]
  in
  if tests = [] then None else Some ("Problem046" >::: tests)


let test_problem_047 =
  let open Problem047 in
  let a, b, c = "a", "b", "c" in
  let mk vars value result =
   (List.map2 (fun x y -> (x, y)) vars value), result
  in
  let tests = [
    "generate" >:: (fun _ ->
        let vars = [a; b] in
        let g = generate vars (And (Var a, Var b)) in
        assert_equal g [
          mk vars [false; false] false;
          mk vars [true; false]  false;
          mk vars [false; true]  false;
          mk vars [true; true]   true
        ]
      );

    "generate" >:: (fun _ ->
        let vars = [a; b; c] in
        let expr =
          (Or(And(Var a, Or(Var b, Var c)),
              Or(And(Var a, Var b), And(Var a, Var c))))
        in
        let g = generate vars expr in
        assert_equal g [
          mk vars [false; false; false] false;
          mk vars [true ; false; false] false;
          mk vars [false; true ; false] false;
          mk vars [true ; true ; false] true ;
          mk vars [false; false; true ] false;
          mk vars [true ; false; true ] true ;
          mk vars [false; true ; true ] false;
          mk vars [true ; true ; true ] true
        ]
      )
  ]
  in
  if tests = [] then None else Some ("Problem047" >::: tests)


let test_problem_048 =
  let open Problem048 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem048" >::: tests)


let test_problem_049 =
  let open Problem049 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem049" >::: tests)


let test_problem_050 =
  let open Problem050 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem050" >::: tests)


let test_problem_051 =
  let open Problem051 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem051" >::: tests)


let test_problem_052 =
  let open Problem052 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem052" >::: tests)


let test_problem_053 =
  let open Problem053 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem053" >::: tests)


let test_problem_054 =
  let open Problem054 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem054" >::: tests)


let test_problem_055 =
  let open Problem055 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem055" >::: tests)


let test_problem_056 =
  let open Problem056 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem056" >::: tests)


let test_problem_057 =
  let open Problem057 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem057" >::: tests)


let test_problem_058 =
  let open Problem058 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem058" >::: tests)


let test_problem_059 =
  let open Problem059 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem059" >::: tests)


let test_problem_060 =
  let open Problem060 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem060" >::: tests)


let test_problem_061 =
  let open Problem061 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem061" >::: tests)


let test_problem_062 =
  let open Problem062 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem062" >::: tests)


let test_problem_063 =
  let open Problem063 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem063" >::: tests)


let test_problem_064 =
  let open Problem064 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem064" >::: tests)


let test_problem_065 =
  let open Problem065 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem065" >::: tests)


let test_problem_066 =
  let open Problem066 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem066" >::: tests)


let test_problem_067 =
  let open Problem067 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem067" >::: tests)


let test_problem_068 =
  let open Problem068 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem068" >::: tests)


let test_problem_069 =
  let open Problem069 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem069" >::: tests)


let test_problem_070 =
  let open Problem070 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem070" >::: tests)


let test_problem_071 =
  let open Problem071 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem071" >::: tests)


let test_problem_072 =
  let open Problem072 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem072" >::: tests)


let test_problem_073 =
  let open Problem073 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem073" >::: tests)


let test_problem_074 =
  let open Problem074 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem074" >::: tests)


let test_problem_075 =
  let open Problem075 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem075" >::: tests)


let test_problem_076 =
  let open Problem076 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem076" >::: tests)


let test_problem_077 =
  let open Problem077 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem077" >::: tests)


let test_problem_078 =
  let open Problem078 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem078" >::: tests)


let test_problem_079 =
  let open Problem079 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem079" >::: tests)


let test_problem_080 =
  let open Problem080 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem080" >::: tests)


let test_problem_081 =
  let open Problem081 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem081" >::: tests)


let test_problem_082 =
  let open Problem082 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem082" >::: tests)


let test_problem_083 =
  let open Problem083 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem083" >::: tests)


let test_problem_084 =
  let open Problem084 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem084" >::: tests)


let test_problem_085 =
  let open Problem085 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem085" >::: tests)


let test_problem_086 =
  let open Problem086 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem086" >::: tests)


let test_problem_087 =
  let open Problem087 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem087" >::: tests)


let test_problem_088 =
  let open Problem088 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem088" >::: tests)


let test_problem_089 =
  let open Problem089 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem089" >::: tests)


let test_problem_090 =
  let open Problem090 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem090" >::: tests)


let test_problem_091 =
  let open Problem091 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem091" >::: tests)


let test_problem_092 =
  let open Problem092 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem092" >::: tests)


let test_problem_093 =
  let open Problem093 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem093" >::: tests)


let test_problem_094 =
  let open Problem094 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem094" >::: tests)


let test_problem_095 =
  let open Problem095 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem095" >::: tests)


let test_problem_096 =
  let open Problem096 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem096" >::: tests)


let test_problem_097 =
  let open Problem097 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem097" >::: tests)


let test_problem_098 =
  let open Problem098 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem098" >::: tests)


let test_problem_099 =
  let open Problem099 in
  let tests = [

  ]
  in
  if tests = [] then None else Some ("Problem099" >::: tests)



(* Tests launcher *)
let run_test_opt = function
  | None -> ()
  | Some t -> ignore (run_test_tt ~verbose:false t)

let _ =
  run_test_opt test_problem_001;
  run_test_opt test_problem_002;
  run_test_opt test_problem_003;
  run_test_opt test_problem_004;
  run_test_opt test_problem_005;
  run_test_opt test_problem_006;
  run_test_opt test_problem_007;
  run_test_opt test_problem_008;
  run_test_opt test_problem_009;
  run_test_opt test_problem_010;
  run_test_opt test_problem_011;
  run_test_opt test_problem_012;
  run_test_opt test_problem_013;
  run_test_opt test_problem_014;
  run_test_opt test_problem_015;
  run_test_opt test_problem_016;
  run_test_opt test_problem_017;
  run_test_opt test_problem_018;
  run_test_opt test_problem_019;
  run_test_opt test_problem_020;
  run_test_opt test_problem_021;
  run_test_opt test_problem_022;
  run_test_opt test_problem_023;
  run_test_opt test_problem_024;
  run_test_opt test_problem_025;
  run_test_opt test_problem_026;
  run_test_opt test_problem_027;
  run_test_opt test_problem_028;
  run_test_opt test_problem_029;
  run_test_opt test_problem_030;
  run_test_opt test_problem_031;
  run_test_opt test_problem_032;
  run_test_opt test_problem_033;
  run_test_opt test_problem_034;
  run_test_opt test_problem_035;
  run_test_opt test_problem_036;
  run_test_opt test_problem_037;
  run_test_opt test_problem_038;
  run_test_opt test_problem_039;
  run_test_opt test_problem_040;
  run_test_opt test_problem_041;
  run_test_opt test_problem_042;
  run_test_opt test_problem_043;
  run_test_opt test_problem_044;
  run_test_opt test_problem_045;
  run_test_opt test_problem_046;
  run_test_opt test_problem_047;
  run_test_opt test_problem_048;
  run_test_opt test_problem_049;
  run_test_opt test_problem_050;
  run_test_opt test_problem_051;
  run_test_opt test_problem_052;
  run_test_opt test_problem_053;
  run_test_opt test_problem_054;
  run_test_opt test_problem_055;
  run_test_opt test_problem_056;
  run_test_opt test_problem_057;
  run_test_opt test_problem_058;
  run_test_opt test_problem_059;
  run_test_opt test_problem_060;
  run_test_opt test_problem_061;
  run_test_opt test_problem_062;
  run_test_opt test_problem_063;
  run_test_opt test_problem_064;
  run_test_opt test_problem_065;
  run_test_opt test_problem_066;
  run_test_opt test_problem_067;
  run_test_opt test_problem_068;
  run_test_opt test_problem_069;
  run_test_opt test_problem_070;
  run_test_opt test_problem_071;
  run_test_opt test_problem_072;
  run_test_opt test_problem_073;
  run_test_opt test_problem_074;
  run_test_opt test_problem_075;
  run_test_opt test_problem_076;
  run_test_opt test_problem_077;
  run_test_opt test_problem_078;
  run_test_opt test_problem_079;
  run_test_opt test_problem_080;
  run_test_opt test_problem_081;
  run_test_opt test_problem_082;
  run_test_opt test_problem_083;
  run_test_opt test_problem_084;
  run_test_opt test_problem_085;
  run_test_opt test_problem_086;
  run_test_opt test_problem_087;
  run_test_opt test_problem_088;
  run_test_opt test_problem_089;
  run_test_opt test_problem_090;
  run_test_opt test_problem_091;
  run_test_opt test_problem_092;
  run_test_opt test_problem_093;
  run_test_opt test_problem_094;
  run_test_opt test_problem_095;
  run_test_opt test_problem_096;
  run_test_opt test_problem_097;
  run_test_opt test_problem_098;
  run_test_opt test_problem_099
