let compress l =
  let rec aux = function
    | (x :: (y :: _ as xs)) ->
        if x <> y then x :: aux xs else aux xs
    | x -> x
  in aux l

let _ = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
