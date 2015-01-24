let slice slist spos epos =
  let in_p x = spos <= x && x <= epos in
  let rec aux cpt = function
    | [] -> []
    | x :: xs when in_p cpt -> x :: aux (cpt + 1) xs
    | _ :: xs when cpt > epos -> [] (* Cut *)
    | _ :: xs -> aux (cpt + 1) xs
  in aux 0 slist
