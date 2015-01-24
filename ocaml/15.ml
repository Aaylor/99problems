let replicate rlist nb =
  let rec cst acc elem cpt =
    if cpt = 0 then acc else cst (elem :: acc) elem (cpt - 1)
  in let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (cst acc x nb) xs
  in List.rev (aux [] rlist)
