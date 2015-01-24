type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec decode = function
  | [] -> []
  | x :: xs ->
    begin match x with
    | One a -> a :: decode xs
    | Many (i, a) ->
        if i = 0 then decode xs else a :: decode (Many(i - 1, a) :: xs)
    end

let _ = decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
