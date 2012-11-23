(* extract sub-cases from apply_gene_no_random *)
(*INPUT
120
1 0.302333 0.425344 1.
1 0.147843 0.500465 1.
1 0.521444 -0.020462 1.
1 -0.521444 -0.020463 1.
1 -0.268072 -0.447727 1.
1 0.517699 -0.065643 1.
1 -0.499834 0.149960 1.
1 0.022669 0.521352 1.
1 0.063453 -0.517972 1.
1 -0.398923 -0.336424 1.
1 -0.486546 -0.188665 1.
1 -0.302332 0.425342 1.
1 -0.521443 -0.020462 1.
2 0.317416 0.002003 0.349591
3 0.317416 0.002003 0.286532
4 0.317416 0.002003 0.0100913
5 0.317416 0.002003 0.0804772 0.0112989 0.866422
6 0.317416 0.002003 0.273309
1 0.057978 -0.712707 1.
1 -0.715061 0.001075 1.
1 0.057979 -0.712707 1.
1 0.705485 0.116635 1.
1 0.483506 -0.526815 1.
1 -0.676665 -0.231163 1.
1 0.483506 -0.526815 1.
1 0.705130 -0.118755 1.
1 -0.705130 -0.118755 1.
1 -0.628365 -0.341276 1.
2 0.209684 -0.215965 0.349591
3 0.209684 -0.215965 0.286532
4 0.209684 -0.215965 0.0100913
5 0.209684 -0.215965 0.0804772 0.0112989 0.866422
6 0.209684 -0.215965 0.273309
2 -0.814469 0.898243 0.349591
3 -0.814469 0.898243 0.286532
4 -0.814469 0.898243 0.0100913
5 -0.814469 0.898243 0.0804772 0.0112989 0.866422
6 -0.814469 0.898243 0.273309
1 0.269480 -0.854036 1.
1 0.737218 -0.508437 1.
1 0.673855 0.589845 1.
1 -0.810848 0.380160 1.
1 -0.022426 -0.895262 1.
1 0.532183 -0.720263 1.
1 -0.890353 0.096281 1.
2 0.823081 -0.776845 0.349591
3 0.823081 -0.776845 0.286532
4 0.823081 -0.776845 0.0100913
5 0.823081 -0.776845 0.0804772 -0.265128 -0.613688
6 0.823081 -0.776845 0.273309
1 0.751272 -0.253933 1.
1 0.436891 -0.661831 1.
1 -0.321998 0.724713 1.
1 0.198322 -0.767829 1.
1 0.751272 -0.253934 1.
1 -0.695648 -0.380744 1.
1 0.628113 -0.484113 1.
1 0.430590 0.665946 1.
1 0.623491 0.490051 1.
1 -0.781583 -0.134238 1.
1 0.793018 0.003762 1.
1 0.748828 0.261050 1.
1 0.430590 0.665946 1.
1 -0.321997 0.724713 1.
1 -0.534328 -0.585990 1.
1 0.623491 0.490050 1.
1 0.751271 -0.253934 1.
1 0.436888 -0.661830 1.
1 0.198321 -0.767827 1.
1 0.751271 -0.253934 1.
1 -0.321996 0.724712 1.
1 0.436888 -0.661830 1.
1 0.793017 0.003762 1.
1 -0.781582 -0.134237 1.
1 -0.534328 -0.585990 1.
1 0.748828 0.261051 1.
1 -0.061737 -0.790619 1.
1 -0.781582 -0.134237 1.
1 0.198322 -0.767826 1.
1 0.623490 0.490049 1.
1 0.430589 0.665943 1.
1 0.191027 0.769672 1.
1 0.623490 0.490048 1.
1 -0.534327 -0.585989 1.
1 -0.699226 0.374125 1.
1 -0.781580 -0.134236 1.
1 -0.782819 0.126816 1.
1 0.751269 -0.253933 1.
1 0.436888 -0.661829 1.
1 0.751269 -0.253935 1.
1 0.436888 -0.661830 1.
1 0.623490 0.490049 1.
1 -0.069235 0.789997 1.
1 0.793016 0.003761 1.
1 -0.539862 0.580893 1.
1 0.623490 0.490048 1.
1 -0.321996 0.724711 1.
1 0.191027 0.769673 1.
1 -0.191027 0.769673 1.
1 0.534327 -0.585989 1.
1 0.321996 0.724711 1.
1 -0.748825 0.261050 1.
1 -0.430588 0.665944 1.
1 -0.436888 -0.661827 1.
1 -0.751270 -0.253932 1.
1 0.782820 0.126814 1.
2 -0.697676 0.943369 0.349591
3 -0.697676 0.943369 0.286532
4 -0.697676 0.943369 0.0100913
5 -0.697676 0.943369 0.0804772 0.0112989 0.866422
6 -0.697676 0.943369 0.273309
1 0.180859 -0.830992 1.
1 0.813218 0.248864 1.
1 -0.843088 -0.111618 1.
1 0.180858 -0.830991 1.
1 0.440881 -0.727241 1.
1 -0.041615 0.849426 1.
1 -0.554568 0.644756 1.
1 0.688349 0.499430 1.
1 -0.315169 0.789888 1.
*)
let apply_func fn_coeff x y=
  let (fn,coeffs) = fn_coeff in
(
 match fn with
 1 -> (
   (* linear *)
   let weight::[]=coeffs in
   (x*.weight,y*.weight)
  )
 |2 ->
  (
   (* sinusoid *)
   let weight::[]=coeffs in
   ((sin x)*.weight,(sin y)*.weight)
  )
 |3 ->
  (
   (* fisheye *)
   let weight::[]=coeffs in
   let tmp=sqrt (x*.x+.y*.y) in
   let r=2.0*.weight /. (tmp+.1.0) in
   (r*.y,r*.x)
  )
 |4 ->
  (
   (* exponential *)
   let weight::[]=coeffs in
   let dx=weight*.(exp (x-.1.0)) in
   let dy=y*.3.141592653 in
   let nx=(cos dy)*.dx in
   let ny=(sin dy)*.dx in
   (nx,ny)
  )
 |5 ->
  (
   (* bent2 *)
   let weight::bx::by::[]=coeffs in
   let nx=if x<0.0 then x*.bx else x in
   let ny=if y<0.0 then y*.by else y in
   (weight*.nx,weight*.ny)
  )
 |6 ->
  (
   (* sec *)
   let weight::[]=coeffs in
   let sinx=sin x in
   let cosx=cos x in
   let sinhy=sinh y in
   let coshy=cosh y in
   let v=2.0 /. ((cos (2.0*.x)) +. (cosh (2.0*.y))) in
   (weight*.v*.cosx*.coshy,weight*.v*.sinx*.sinhy)
  )
 |10 ->
  (
   (* perspective *)
   let weight::angle::dist::[]=coeffs in
   let ang=angle*.3.14159265*.0.5 in
   let vsin=sin ang in
   let vfcos=dist*.(cos ang) in
   let t=1.0/.(dist-.y*.vsin) in
   (weight*.dist*.x*.t,weight*.vfcos*.y*.t)
  )
 |11 ->
  (
   (* spherical *)
    let weight::[]=coeffs in
    let r2=weight/.(x*.x+.y*.y+. 1e-10) in
    (r2*.x,r2*.y)
  )
 |12 ->
  (
   (* swirl *)
    let weight::[]=coeffs in
    let r2=x*.x+.y*.y in
    let c1,c2=sin r2,cos r2 in
    let nx,ny=c1*.x-.c2*.y , c2*.x+.c1*.y in
    (weight*.nx,weight*.ny)
  )
 |13 ->
  (
   (*eyefish*)
    let weight::[]=coeffs in
    let r=(weight*.2.0)/.(sqrt ((x*.x+.y*.y) +. 1.0)) in
    (r*.x,r*.y)
  )
 |16 ->
 (
  (* fn16:disc *)
  let weight::[]=coeffs in
  let a=(atan2 x y)*.0.31830988618 in
  let r=3.14159265*.(sqrt ((x*.x)+.(y*.y))) in
  let sr,cr=sin r,cos r in
  (weight*.sr*.a,weight*.cr*.a)
 )
 |18 ->
 (
  (* fn18:curl/curl_c1/curl_c2 *)
   let weight::c1::c2::[]=coeffs in
   let re=1.0+.c1*.x+.c2*.(x*.x-.y*.y) in
   let im=c1*.y+.2.0*.c2*.x*.y in
   let r=weight/.(re*.re+.im*.im) in
   ((x*.re+.y*.im)*.r,(y*.re-.x*.im)*.r)
 )
 |19 ->
 (
  (* fn19:rectangles/rectangles_x/rectangles_y *)
   let weight::rect_x::rect_y::[]=coeffs in
   ((if rect_x==0.0 then weight*.x else weight*.((2.0*.(floor (x/.rect_x))+.1.0)*.rect_x-.x)),
    (if rect_y==0.0 then weight*.y else weight*.((2.0*.(floor (y/.rect_y))+.1.0)*.rect_y-.y)))
 )
 |21 ->
 (
   (* fn21:waves/c0/c1/waves_dx2/waves_dy2 *)
   let weight::c0::c1::dx2::dy2::[]=coeffs in
   let nx,ny=x+.c0*.(sin (y*.dx2)),y+.c1*.(sin (x*.dy2)) in
   (weight*.nx,weight*.ny)
 )
 |25 ->
 (
  (*25 fan2::x::y*)
  let weight::fanX::fanY::[]=coeffs in
  let dx=3.14159265*.(x*.x+.(1e-10)) in
  let dx2=0.5*.dx in
  let a=atan2 x y in
  let r=weight*.(sqrt (x*.x+.y*.y)) in
  let t=a+.y-.dx*.(floor (((a+.y)/.dx)+.0.5)) in
  let a2=if t>dx2 then a-.dx2 else a+.dx2 in
  let sa=sin a2 in
  let ca=cos a2 in
  (r*.sa,r*.ca)
 )
 |26 ->
 (
  (*26 disc2::rot::twist *)
  let weight::rot::twist::timespi::cosadd::sinadd::[]=coeffs in
  let t=timespi*.(x+.y) in
  let sinr=sin t in
  let cosr=cos t in
  let r=weight*.(atan2 x y)/.3.14159265 in
  ((sinr+.cosadd)*.r,(cosr+.sinadd)*.r)
 )
 |28 ->
 (
  (*28 rings*)
  let fmod p q=
    let r=(p/.q) in
    let s=if r>=0.0 then 1.0 else (-1.0) in
    let r_af=floor (abs_float r) in
    p-.(q*.r_af)*.s
  in
  let weight::c::[]=coeffs in
  let dx = c*.c+.(1e-10) in
  let r = sqrt ((x*.x)+.(y*.y)) in
  let a=atan2 x y in
  let r2 = weight*.(fmod (r+.dx) (2.0*.dx)-.dx+.r*.(1.0-.dx))in
  let cosa,sina=(cos a)*.r2,(sin a)*.r2 in
  (weight*.cosa,weight*.sina)
 )
 |29 ->
 (
  (*29 polar *)
  let weight::[]=coeffs in
  let nx=(atan2 x y)*.0.318309886 in
  let ny=(sqrt (x*.x+.y*.y))-.1.0 in
  (weight*.nx,weight*.ny)
 )
 |_ ->
   (0.0,0.0)
)
in
let test () =
  let fn = read_int () in
  let x = read_float () in
  let y = read_float () in
  let rec read_coeffs n =
    if n = 0 then [] else
      let c = read_float () in
      c :: (read_coeffs (n-1))
  in
  let coeff_count =
    if fn = 5 or fn = 10 or fn = 18 or fn = 19 or fn = 25 then 3
    else if fn = 21 then 5
    else if fn = 26 then 6
    else if fn = 28 then 2
    else 1
  in
  let (x, y) = apply_func (fn, read_coeffs coeff_count) x y in
  print_int5 x;
  print_int5 y
in
let rec test_iter n =
  if n <= 0 then ()
  else (test (); test_iter (n-1))
in
test_iter (read_int ())
