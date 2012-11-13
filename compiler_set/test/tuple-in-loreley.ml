let rec f coeffs =
  let x, y = 3., 4. in
  let weight::c0::c1::dx2::dy2::[]=coeffs in
  let nx,ny=x+.c0*.(sin (y*.dx2)),y+.c1*.(sin (x*.dy2)) in
  (weight*.nx,weight*.ny)
in
print_tuple (f (1. :: 2. :: 3. :: 4. :: 5. :: []))
