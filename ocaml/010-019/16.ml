let drop rlist size =
  let rec aux cpt = function
    | [] -> []
    | x :: xs when cpt > 1 -> x :: aux (cpt - 1) xs
    | x :: xs -> aux size xs
  in aux size rlist
