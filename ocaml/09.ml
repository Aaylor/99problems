let pack : string list -> string list list =
  fun slist ->
    let rec aux acc pack = function
      | []  -> pack
      | [x] -> (x :: acc) :: pack
      | x :: ((y :: _) as l) ->
        if x = y then aux (x :: acc) pack l
        else aux [] ((x :: acc) :: pack) l
    in aux [] [] slist |> List.rev

let _ = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"f"]
