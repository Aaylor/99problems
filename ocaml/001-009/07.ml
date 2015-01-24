type 'a node =
  | One  of 'a
  | Many of 'a node list

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One  i :: xs -> aux (i :: acc)   xs
    | Many m :: xs -> aux (aux acc m)  xs
  in aux [] l |> List.rev
