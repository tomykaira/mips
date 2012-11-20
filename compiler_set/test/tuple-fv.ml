let rec f y =
  let x = (1, y) in
  let rec g z =
    let (s, t) = x in
    if z = 0 then s else t
  in
  g 1
in
f 2
