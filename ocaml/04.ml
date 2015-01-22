let length l =
  let rec aux n = function
    | []      -> n
    | x :: xs -> aux (n + 1) xs
  in aux 0 l
