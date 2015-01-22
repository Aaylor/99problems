let encode : string list -> (int * string) list =
  fun slist ->
    let rec count acc current_count = function
      | []  -> acc
      | [x] -> (current_count + 1, x) :: acc
      | x :: ((y :: _) as l) ->
          if x = y then count acc (current_count + 1) l
          else count ((current_count, x) :: acc) 1 l
    in count [] 1 slist |> List.rev


  let _ = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
