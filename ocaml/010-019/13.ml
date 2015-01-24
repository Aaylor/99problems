type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode : string list -> string rle list =
  function slist ->
    let to_rle c x = if c = 1 then One x else Many (c, x) in
    let rec count acc current_count = function
      | []  -> acc
      | [x] -> to_rle (current_count + 1) x :: acc
      | x :: ((y :: _) as l) ->
          if x = y then count acc (current_count + 1) l
          else count (to_rle current_count x :: acc) 1 l
    in count [] 1 slist |> List.rev

let _ = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
