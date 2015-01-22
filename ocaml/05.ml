let rev l =
  let rec aux acc = function
    | []      -> acc
    | x :: xs -> aux (x :: acc) xs
  in aux [] l
