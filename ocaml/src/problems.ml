


(* Problem 001 *) 
module Problem001 = struct

  let rec last = function
    | []      -> None
    | [x]     -> Some x
    | x :: xs -> last xs

end


(* Problem 002 *) 
module Problem002 = struct

  let rec last_two = function
    | [] | [_] -> None
    | [x; y]   -> Some (x, y)
    | _ :: xs  -> last_two xs

end


(* Problem 003 *) 
module Problem003 = struct

  let rec at x = function
    | [] -> None
    | y :: ys when x = 1 -> Some y
    | _ :: ys -> at (x - 1) ys

end


(* Problem 004 *) 
module Problem004 = struct

  let length l =
    let rec aux n = function
      | []      -> n
      | x :: xs -> aux (n + 1) xs
    in aux 0 l

end


(* Problem 005 *) 
module Problem005 = struct

  let rev l =
    let rec aux acc = function
      | []      -> acc
      | x :: xs -> aux (x :: acc) xs
    in aux [] l

end


(* Problem 006 *) 
module Problem006 = struct

  let palindrome l = l = Problem005.rev l

end


(* Problem 007 *) 
module Problem007 = struct

  type 'a node =
    | One  of 'a
    | Many of 'a node list
  
  let flatten l =
    let rec aux acc = function
      | [] -> acc
      | One  i :: xs -> aux (i :: acc)   xs
      | Many m :: xs -> aux (aux acc m)  xs
    in aux [] l |> List.rev

end


(* Problem 008 *) 
module Problem008 = struct

  let compress l =
    let rec aux = function
      | (x :: (y :: _ as xs)) ->
        if x <> y then x :: aux xs else aux xs
      | x -> x
    in aux l
 
end


(* Problem 009 *) 
module Problem009 = struct

  let pack slist =
    let rec aux acc pack = function
      | []  -> pack
      | [x] -> (x :: acc) :: pack
      | x :: ((y :: _) as l) ->
        if x = y then aux (x :: acc) pack l
        else aux [] ((x :: acc) :: pack) l
    in List.rev (aux [] [] slist)

end


(* Problem 010 *) 
module Problem010 = struct

  let encode slist =
    let rec count acc current_count = function
      | []  -> acc
      | [x] -> (current_count, x) :: acc
      | x :: ((y :: _) as l) ->
        if x = y then count acc (current_count + 1) l
        else count ((current_count, x) :: acc) 1 l
    in List.rev (count [] 1 slist)

end


(* Problem 011 *) 
module Problem011 = struct

  type 'a rle =
    | One of 'a
    | Many of int * 'a

  let encode slist =
    let to_rle c x = if c = 1 then One x else Many (c, x) in
    let rec count acc current_count = function
      | []  -> acc
      | [x] -> to_rle current_count x :: acc
      | x :: ((y :: _) as l) ->
        if x = y then count acc (current_count + 1) l
        else count (to_rle current_count x :: acc) 1 l
    in List.rev (count [] 1 slist)

end


(* Problem 012 *) 
module Problem012 = struct

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

end


(* Problem 013 *) 
module Problem013 = struct

  type 'a rle =
    | One of 'a
    | Many of int * 'a

  let encode slist =
    let to_rle c x = if c = 1 then One x else Many (c, x) in
    let rec count acc current_count = function
      | []  -> acc
      | [x] -> to_rle current_count x :: acc
      | x :: ((y :: _) as l) ->
        if x = y then count acc (current_count + 1) l
        else count (to_rle current_count x :: acc) 1 l
    in List.rev (count [] 1 slist)

end


(* Problem 014 *) 
module Problem014 = struct

  let rec duplicate = function
    | [] -> []
    | x :: xs -> x :: x :: duplicate xs

end


(* Problem 015 *) 
module Problem015 = struct

  let replicate rlist nb =
    let rec cst acc elem cpt =
      if cpt = 0 then acc else cst (elem :: acc) elem (cpt - 1)
    in
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (cst acc x nb) xs
    in
    List.rev (aux [] rlist)
  
end


(* Problem 016 *) 
module Problem016 = struct

  let drop rlist size =
  let rec aux cpt = function
    | [] -> []
    | x :: xs when cpt > 1 -> x :: aux (cpt - 1) xs
    | x :: xs -> aux size xs
  in aux size rlist

end


(* Problem 017 *) 
module Problem017 = struct

  let split rlist size =
    let rec aux s acc = function
      | [] ->
        (List.rev acc, [])
      | x :: xs when s = 0 ->
        (List.rev acc, x :: xs)
      | x :: xs ->
        aux (s - 1) (x :: acc) xs
    in aux size [] rlist

end


(* Problem 018 *) 
module Problem018 = struct

  let slice slist spos epos =
    let in_p x = spos <= x && x <= epos in
    let rec aux cpt = function
      | [] -> []
      | x :: xs when in_p cpt -> x :: aux (cpt + 1) xs
      | _ :: xs when cpt > epos -> [] (* Cut *)
      | _ :: xs -> aux (cpt + 1) xs
    in aux 0 slist

end


(* Problem 019 *) 
module Problem019 = struct

  let rotate rlist n =
    let n = if n < 0 then List.length rlist + n else n in
    let right, left = Problem017.split rlist n in
    left @ right

end


(* Problem 020 *) 
module Problem020 = struct

  let remove_at pos rlist =
    let rec aux cpt = function
      | [] -> []
      | x :: xs when cpt = pos -> xs
      | x :: xs -> x :: aux (cpt + 1) xs 
    in aux 0 rlist

end


(* Problem 021 *) 
module Problem021 = struct

  let insert_at elem pos rlist =
    let rec aux cpt = function
      | [] -> [elem]
      | x :: xs when cpt = pos -> elem :: x :: xs
      | x :: xs -> x :: aux (cpt + 1) xs
    in aux 0 rlist

end


(* Problem 022 *) 
module Problem022 = struct

  let range s e =
    let rec aux p =
      if p = e then [p]
      else p :: aux (if p > e then p - 1 else p + 1)
    in aux s

end


(* Problem 023 *) 
module Problem023 = struct

  exception Invalid_length
  
  let rand_select rlist i = 
    let length = List.length rlist in
    if i > length then raise Invalid_length;
    let rand () = Random.int length in
    let rec get acc = function
      | cpt when cpt = i -> acc
      | cpt ->
        let elem = List.nth rlist (rand ()) in
        match List.exists (( = )  elem) acc with
        | true  -> get acc cpt
        | false -> get (elem :: acc) (cpt + 1)
    in List.rev (get [] 0)

end


(* Problem 024 *) 
module Problem024 = struct
  
  let lotto_select nb max =
    Problem023.rand_select (Problem022.range 1 max) nb
      
end


(* Problem 025 *) 
module Problem025 = struct

  let permutation l = Problem023.rand_select l (List.length l)

end


(* Problem 026 *) 
module Problem026 = struct

  (* write here *)
  
end


(* Problem 027 *) 
module Problem027 = struct

  (* write here *)

end


(* Problem 028 *) 
module Problem028 = struct

  (* write here *)

end


(* Problem 029 *) 
module Problem029 = struct

  (* write here *)

end


(* Problem 030 *) 
module Problem030 = struct

  (* write here *)

end


(* Problem 031 *) 
module Problem031 = struct

  (* write here *)

end


(* Problem 032 *) 
module Problem032 = struct

  (* write here *)

end


(* Problem 033 *) 
module Problem033 = struct

  (* write here *)

end


(* Problem 034 *) 
module Problem034 = struct

  (* write here *)

end


(* Problem 035 *) 
module Problem035 = struct

  (* write here *)

end


(* Problem 036 *) 
module Problem036 = struct

  (* write here *)

end


(* Problem 037 *) 
module Problem037 = struct

  (* write here *)

end


(* Problem 038 *) 
module Problem038 = struct

  (* write here *)

end


(* Problem 039 *) 
module Problem039 = struct

  (* write here *)

end


(* Problem 040 *) 
module Problem040 = struct

  (* write here *)

end


(* Problem 041 *) 
module Problem041 = struct

  (* write here *)

end


(* Problem 042 *) 
module Problem042 = struct

  (* write here *)

end


(* Problem 043 *) 
module Problem043 = struct

  (* write here *)

end


(* Problem 044 *) 
module Problem044 = struct

  (* write here *)

end


(* Problem 045 *) 
module Problem045 = struct

  (* write here *)

end


(* Problem 046 *) 
module Problem046 = struct

  (* write here *)

end


(* Problem 047 *) 
module Problem047 = struct

  (* write here *)

end


(* Problem 048 *) 
module Problem048 = struct

  (* write here *)

end


(* Problem 049 *) 
module Problem049 = struct

  (* write here *)

end


(* Problem 050 *) 
module Problem050 = struct

  (* write here *)

end


(* Problem 051 *) 
module Problem051 = struct

  (* write here *)

end


(* Problem 052 *) 
module Problem052 = struct

  (* write here *)

end


(* Problem 053 *) 
module Problem053 = struct

  (* write here *)

end


(* Problem 054 *) 
module Problem054 = struct

  (* write here *)

end


(* Problem 055 *) 
module Problem055 = struct

  (* write here *)

end


(* Problem 056 *) 
module Problem056 = struct

  (* write here *)

end


(* Problem 057 *) 
module Problem057 = struct

  (* write here *)

end


(* Problem 058 *) 
module Problem058 = struct

  (* write here *)

end


(* Problem 059 *) 
module Problem059 = struct

  (* write here *)

end


(* Problem 060 *) 
module Problem060 = struct

  (* write here *)

end


(* Problem 061 *) 
module Problem061 = struct

  (* write here *)

end


(* Problem 062 *) 
module Problem062 = struct

  (* write here *)

end


(* Problem 063 *) 
module Problem063 = struct

  (* write here *)

end


(* Problem 064 *) 
module Problem064 = struct

  (* write here *)

end


(* Problem 065 *) 
module Problem065 = struct

  (* write here *)

end


(* Problem 066 *) 
module Problem066 = struct

  (* write here *)

end


(* Problem 067 *) 
module Problem067 = struct

  (* write here *)

end


(* Problem 068 *) 
module Problem068 = struct

  (* write here *)

end


(* Problem 069 *) 
module Problem069 = struct

  (* write here *)

end


(* Problem 070 *) 
module Problem070 = struct

  (* write here *)

end


(* Problem 071 *) 
module Problem071 = struct

  (* write here *)

end


(* Problem 072 *) 
module Problem072 = struct

  (* write here *)

end


(* Problem 073 *) 
module Problem073 = struct

  (* write here *)

end


(* Problem 074 *) 
module Problem074 = struct

  (* write here *)

end


(* Problem 075 *) 
module Problem075 = struct

  (* write here *)

end


(* Problem 076 *) 
module Problem076 = struct

  (* write here *)

end


(* Problem 077 *) 
module Problem077 = struct

  (* write here *)

end


(* Problem 078 *) 
module Problem078 = struct

  (* write here *)

end


(* Problem 079 *) 
module Problem079 = struct

  (* write here *)

end


(* Problem 080 *) 
module Problem080 = struct

  (* write here *)

end


(* Problem 081 *) 
module Problem081 = struct

  (* write here *)

end


(* Problem 082 *) 
module Problem082 = struct

  (* write here *)

end


(* Problem 083 *) 
module Problem083 = struct

  (* write here *)

end


(* Problem 084 *) 
module Problem084 = struct

  (* write here *)

end


(* Problem 085 *) 
module Problem085 = struct

  (* write here *)

end


(* Problem 086 *) 
module Problem086 = struct

  (* write here *)

end


(* Problem 087 *) 
module Problem087 = struct

  (* write here *)

end


(* Problem 088 *) 
module Problem088 = struct

  (* write here *)

end


(* Problem 089 *) 
module Problem089 = struct

  (* write here *)

end


(* Problem 090 *) 
module Problem090 = struct

  (* write here *)

end


(* Problem 091 *) 
module Problem091 = struct

  (* write here *)

end


(* Problem 092 *) 
module Problem092 = struct

  (* write here *)

end


(* Problem 093 *) 
module Problem093 = struct

  (* write here *)

end


(* Problem 094 *) 
module Problem094 = struct

  (* write here *)

end


(* Problem 095 *) 
module Problem095 = struct

  (* write here *)

end


(* Problem 096 *) 
module Problem096 = struct

  (* write here *)

end


(* Problem 097 *) 
module Problem097 = struct

  (* write here *)

end


(* Problem 098 *) 
module Problem098 = struct

  (* write here *)

end


(* Problem 099 *) 
module Problem099 = struct

  (* write here *)

end
