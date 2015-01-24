let split rlist size =
  let rec aux s acc = function
    | [] ->
        (List.rev acc, [])
    | x :: xs when s = 0 ->
        (List.rev acc, x :: xs)
    | x :: xs ->
        aux (s - 1) (x :: acc) xs
  in aux size [] rlist
