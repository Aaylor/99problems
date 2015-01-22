let rec at x = function
  | [] -> None
  | y :: ys when x = 1 -> Some y
  | _ :: ys -> at (x - 1) ys
