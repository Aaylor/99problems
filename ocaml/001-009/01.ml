let rec last : 'a list -> 'a option = function
  | []      -> None
  | [x]     -> Some x
  | x :: xs -> last xs
