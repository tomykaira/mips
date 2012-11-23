(*
  Include random, so not comparable with ocaml output
  To debug,
  - random
  - apply_gene multi time
  - logic in shooting_sub
  - cMap
*)
(*INPUT
500 400 1 0 0 1.37083 1.0375 1 0.25 0.01
9 0 0.4

256
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255
0 231 37
1 189 0
16 170 0
21 177 0
33 188 0
55 197 0
0 59 211
0 82 244
25 149 255
65 184 255
100 205 255
140 218 253
155 227 255
153 228 255
133 235 255
119 238 255
99 249 255
86 255 238
80 254 177
105 255 97
202 255 90
247 255 66
255 235 47
253 204 22
255 158 0
242 91 0
227 18 0
255 55 178
241 112 255
211 167 255
155 110 255
47 143 255

21 0

0 0.5 0.5 5
-0.0557357 0.296215 0.345357 0.819319 0.740205 -0.591994
-0.212083 0.653373 -0.458351 0.589448 -0.535298 0.526096
1 2 0.349591
1 3 0.286532
1 4 0.0100913
3 5 0.0804772 -0.265128 -0.613688
1 6 0.273309

1 0.5 0.5 5
-0.848127 0.670267 0.618477 0.456033 -0.112178 0.36084
-0.599703 -0.815963 -0.293803 -0.780384 0.924242 0.114203
1 2 0.349591
1 3 0.286532
1 4 0.0100913
3 5 0.0804772 0.0112989 0.866422
1 6 0.273309

1 0 1 1
-1 0 0 1 0 0
1 0 0 1 0 0
1 1 1

0.235294 0 1 1
-0.082579 0.996584 -0.996584 -0.082579 0 0
1 0 0 1 0 0
1 1 1

0.764706 0 1 1
-0.082579 -0.996584 0.996584 -0.082579 0 0
1 0 0 1 0 0
1 1 1

0.117647 0 1 1
0.546948 0.837166 -0.837166 0.546948 0 0
1 0 0 1 0 0
1 1 1

0.882353 0 1 1
0.546948 -0.837166 0.837166 0.546948 0 0
1 0 0 1 0 0
1 1 1

0.294118 0 1 1
-0.401695 0.915773 -0.915773 -0.401695 0 0
1 0 0 1 0 0
1 1 1

0.705882 0 1 1
-0.401695 -0.915773 0.915773 -0.401695 0 0
1 0 0 1 0 0
1 1 1

0 0 1 1
0.945817 0.324699 -0.324699 0.945817 0 0
1 0 0 1 0 0
1 1 1

1 0 1 1
0.945817 -0.324699 0.324699 0.945817 0 0
1 0 0 1 0 0
1 1 1

0.176471 0 1 1
0.245485 0.9694 -0.9694 0.245485 0 0
1 0 0 1 0 0
1 1 1

0.823529 0 1 1
0.245485 -0.9694 0.9694 0.245485 0 0
1 0 0 1 0 0
1 1 1

0.470588 0 1 1
-0.986361 0.164595 -0.164595 -0.986361 0 0
1 0 0 1 0 0
1 1 1

0.529412 0 1 1
-0.986361 -0.164595 0.164595 -0.986361 0 0
1 0 0 1 0 0
1 1 1

0.411765 0 1 1
-0.879474 0.475947 -0.475947 -0.879474 0 0
1 0 0 1 0 0
1 1 1

0.588235 0 1 1
-0.879474 -0.475947 0.475947 -0.879474 0 0
1 0 0 1 0 0
1 1 1

0.352941 0 1 1
-0.677282 0.735724 -0.735724 -0.677282 0 0
1 0 0 1 0 0
1 1 1

0.647059 0 1 1
-0.677282 -0.735724 0.735724 -0.677282 0 0
1 0 0 1 0 0
1 1 1

0.0588235 0 1 1
0.789141 0.614213 -0.614213 0.789141 0 0
1 0 0 1 0 0
1 1 1

0.941176 0 1 1
0.789141 -0.614213 0.614213 0.789141 0 0
1 0 0 1 0 0
1 1 1


788800 0
*)

let random_source =
  let rs = Array.create 4 0 in
  rs.(0) <- 123456789; 
  rs.(1) <- 362436069;
  rs.(2) <- 521288629; 
  rs.(3) <- 88675123;
  rs
in

let rec xor128 _ =
  let t = xor random_source.(0) ((lsl) random_source.(0) 11) in
  random_source.(0) <- random_source.(1);
  random_source.(1) <- random_source.(2);
  random_source.(2) <- random_source.(3);
  random_source.(3) <- xor (xor random_source.(3) ((lsr) random_source.(3) 19)) (xor t ((lsr) t 8));
  random_source.(3)
in

let rec random_float max=
  (float_of_int ((land) (xor128 ()) 8388607)) /. (float_of_int 8388607) *. max
in

let rec random_int max=
  ((mod) (abs ((land) (xor128 ()) (((lsl) 1 31) - 1))) max)
in

let conf              = Array.create 10 (0.0) in
let bound             = Array.create 4 (0.0) in
let dummy             = Array.create 4 0.0 in
let tbl               = ref (Array.create 1 dummy) in
let cMap              = ref (Array.create 1 (0.0,0.0,0.0)) in
let cConf             = Array.create 1 256 in
let deConf            = Array.create 3 0.0 in
let dnaArray          = ref (Array.create 0 (0., 0., 0., (0., 0., 0., 0., 0., 0.), [], (0., 0., 0., 0., 0., 0.))) in
let finalXform        = ref (0.0,0.0,0.0,(0.0,0.0,0.0,0.0,0.0,0.0),[],(0.0,0.0,0.0,0.0,0.0,0.0)) in
let genes             = ref 0 in
let enable_finalXform = ref 0 in
let funcRandTblSize   = 1024 in
let funcRandTbl       = Array.create funcRandTblSize 0 in
let glConf            = Array.create 2 0 in

(* 以下は計算用ロジック *)
let rec gene_gen res=
  if res==0 then []
  else 
  (
   let head=read_float () in
   head::(gene_gen (res - 1))
  )
in

let rec dna_gen res=
  if res==0 then []
  else
  (
  let len=read_int () in
  let fType=read_int () in
  let head=(fType,gene_gen len) in
  head::(dna_gen (res - 1))
  )
in

let read_dnas n=
let cIndex=read_float () in
let cBlendRate=read_float () in
let cWeight=read_float () in
let num_dna=read_int () in
let c1=read_float () in
let c2=read_float () in
let c3=read_float () in
let c4=read_float () in
let c5=read_float () in
let c6=read_float () in
let d1=read_float () in
let d2=read_float () in
let d3=read_float () in
let d4=read_float () in
let d5=read_float () in
let d6=read_float () in
(cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),dna_gen num_dna,(d1,d2,d3,d4,d5,d6))
in

let read_gene ()=
(genes := read_int();enable_finalXform:=read_int();
dnaArray:=Array.init (!genes) read_dnas;
if !enable_finalXform ==1 then (finalXform:=read_dnas 0) else ())
in

let rec read_cMap_sub iter=
(
 let r=read_float () in
 let g=read_float () in
 let b=read_float () in
 !cMap.(iter) <- (r,g,b);
 if iter==(cConf.(0)-1) then ()
 else read_cMap_sub (iter + 1)
)
in

let read_cMap ()=
(
 cConf.(0) <- read_int ();
 cMap:=Array.make cConf.(0) (0.0,0.0,0.0);
 read_cMap_sub 0
)
in

let array_gen x=
 Array.make 4 0.0
in

let decode_init ()=
  let rec acmWeight pos min sum=
    if pos==(!genes) then (min,sum) else
    let (cIndex,cBlendRate,cWeight,_,fun_ary,_)= !dnaArray.(pos) in
    acmWeight (pos+1) (if min>cWeight then cWeight else min) (sum+.cWeight)
  in
  let (min, sum) = acmWeight 0 1000000.0 0.0 in
  (let rec initRandTbl_sub v pos rest=
     if rest==0 then pos else
     (funcRandTbl.(pos)<-v;initRandTbl_sub v (pos+1) (rest-1))
   in
   let rec initRandTbl fn index=
     if fn==(!genes) then () else
     (let (cIndex,cBlendRate,cWeight,_,fun_ary,_)= !dnaArray.(fn) in
     initRandTbl (fn+1) (initRandTbl_sub fn index (int_of_float (cWeight*.(float_of_int funcRandTblSize)/.sum)))) in
   initRandTbl 0 0)
in

let rec read_environment pos=
(
 conf.(pos) <- read_float ();
 if pos==9 then 
 (
  bound.(0) <- (conf.(3) -. conf.(5) *. conf.(7));
  bound.(1) <- (conf.(3) +. conf.(5) *. conf.(7));
  bound.(2) <- (conf.(4) -. conf.(6) *. conf.(7));
  bound.(3) <- (conf.(4) +. conf.(6) *. conf.(7));
  let ret_width=(conf.(0)+.conf.(2)*.6.0) in
  let ret_height=(conf.(1)+.conf.(2)*.6.0) in
  tbl := (Array.init (int_of_float (ret_width*.ret_height)) array_gen)
 ) 
 else 
  read_environment (pos + 1)
)
in

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

 |7 ->
  (
   (* juliaScope *)
   let weight::power::dist::[]=coeffs in
   let rN=abs_float power in
   let cn=dist/.power*.0.5 in
   let t_rnd=(floor ((random_float 1.0)*.rN)) in
   let tmpr=(2.0*.3.14159265*.t_rnd+.(if ((land) (int_of_float t_rnd) 1) ==0 then (atan2 y x) else 0.0-.(atan2 y x)))/.power in
   let r=weight*.((x*.x+.y*.y) ** cn) in
   let sina,cosa=sin tmpr,cos tmpr in
   (r*.cosa,r*.sina)
  )
 |8 ->
  (
   (* juliaN *)
   let weight::power::dist::[]=coeffs in
   let rN=abs_float power in
   let cn=dist/.power*.0.5 in
   let t_rnd=floor ((random_float 1.0)*.rN) in
   let tmpr=((atan2 y x)+.2.0*.3.14159265*.t_rnd)/.power in
   let r=weight*.((x*.x+.y*.y) ** cn) in
   let sina,cosa=sin tmpr,cos tmpr in
   (r*.cosa,r*.sina)
  )
 |9 ->
  (
   (* gaussian_blur *)
   let weight::[]=coeffs in
   let ang=(random_float 1.0) *. 2.0 *. 3.14159265 in
   let sina,cosa=sin ang,cos ang in
   let r=weight*.((random_float 1.0) +. (random_float 1.0) +. (random_float 1.0) +. (random_float 1.0) -. 2.0) in
   (r*.cosa,r*.sina)
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
 |14 ->
  (
   (*julia*)
   let weight::[]=coeffs in
   let a=(atan2 x y)*.0.5+.(if random_int 2==0 then 3.14159265 else 0.0) in
   let r=weight*.((x*.x+.y*.y) **0.25) in
   let ca,sa=cos a,sin a in
   (r*.ca,r*.sa)
  )
 |15 ->
 (
  (* fn15:blur *)
  let weight::[]=coeffs in
  let tmpr=(random_float 1.0)*.2.0*.3.14159265 in
  let sinr,cosr=sin tmpr,cos tmpr in
  let r=weight*.(random_float 1.0) in
  (r*.cosr,r*.sinr)
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
 |22 ->
 (
  (*22 flower weight::petals::holes*)
  let weight::petals::holes::[]=coeffs in
  let theta=atan2 y x in
  let r=weight*.((random_float 1.0)-.holes)*.(cos (petals*.theta))/.(sqrt (x*.x+.y*.y)) in
  (r*.x,r*.y)
 )
 |24 ->
 (
  (*24 conic::eccentricity::holes*)
  let weight::eccentricity::holes::[]=coeffs in
  let sq=sqrt (x*.x+.y*.y) in
  let ct=x/.sq in
  let r=weight*.((random_float 1.0)-.holes)*.eccentricity/.(1.0+.eccentricity*.ct)/.sq in
  (r*.x,r*.y)
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
 |27 ->
 (
  (*27 parabola::height::width*)
  let weight::p_height::p_width::[]=coeffs in
  let r = sqrt (x*.x+.y*.y) in
  let sr,cr=sin r,cos r in
  (p_height*.weight*.sr*.sr*.(random_float 1.0),p_width*.weight*.cr*.(random_float 1.0))
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

let rec apply_gene_sub fn_ary x y retx rety=
  if fn_ary==[] then (retx,rety)
  else
    (
     let head::tail=fn_ary in
       (let (nx,ny)=apply_func head x y in
       apply_gene_sub tail x y (retx+.nx) (rety+.ny))
    )
in

let apply_gene fn x y col=
  let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !dnaArray.(fn) in
  let nx,ny=apply_gene_sub fun_ary (c1*.x+.c3*.y+.c5) (c2*.x+.c4*.y+.c6) 0.0 0.0 in
  ((d1*.nx+.d3*.ny+.d5),(d2*.nx+.d4*.ny+.d6),(col *. (1.0 -. cBlendRate)) +. cIndex*.cBlendRate)
in

let apply_finalXform x y col=
let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !finalXform in
  let nx,ny=apply_gene_sub fun_ary (c1*.x+.c3*.y+.c5) (c2*.x+.c4*.y+.c6) 0.0 0.0 in
  ((d1*.nx+.d3*.ny+.d5),(d2*.nx+.d4*.ny+.d6),(col *. (1.0 -. cBlendRate)) +. cIndex*.cBlendRate)
in

(* 正則性判定 *)
let regular x y=
  not ((x<bound.(0)) or (x>bound.(1)) or (y<bound.(2)) or (y>bound.(3)))
in

(* 
 current_pos: (x,y,col)
 remaining_iteration: iter
 *)
let rec shooting_sub x y col iter=
let fn= funcRandTbl.(random_int funcRandTblSize) in
let tx,ty,tcol=apply_gene fn x y col in
let nx,ny,ncol=if !enable_finalXform==1 then apply_finalXform tx ty tcol else (tx,ty,tcol) in
(
 if regular nx ny then
    (let cIndexf=ncol*.(float_of_int cConf.(0)) in
     let cIndexi=floor cIndexf in
     let frac=cIndexf-.cIndexi in
     let fracn=1.0-.frac in
     let (r1,g1,b1)= !cMap.(int_of_float cIndexi) in(*TODO: add color data into bin*)
     let (r2,g2,b2)= 
              !cMap.(int_of_float 
              (if cIndexi+.1.0>=(float_of_int cConf.(0)) 
               then cIndexi else (cIndexi+.1.0))) in
     let (nr,ng,nb)=(r1*.fracn+.r2*.frac,g1*.fracn+.g2*.frac,b1*.fracn+.b2*.frac) in
     let ret_width=(conf.(0)+.conf.(2)*.6.0) in
     let ret_height=(conf.(1)+.conf.(2)*.6.0) in
     let target_index=(int_of_float (ret_width
                                     /. (bound.(1) -. bound.(0))
                                     *. (nx -. bound.(0))))
                     +(int_of_float ret_width)*
                      (int_of_float (ret_height
                                     /. (bound.(3) -. bound.(2))
                                          *. (ny-.bound.(2))))
     in
     let target= !tbl.(target_index) in
     target.(0) <- target.(0)+.nr;
     target.(1) <- target.(1)+.ng;
     target.(2) <- target.(2)+.nb;
     target.(3) <- target.(3)+.255.0;
     print_int target_index; print_char '\n';
     print_int5 nx;
     print_int5 ny;
     print_int5 nr;
     print_int5 ng;
     print_int5 nb;
     if iter==0 then () else shooting_sub nx ny ncol (iter - 1))
 else
    (if iter==0 then () else shooting_sub nx ny ncol iter)
)
in
let read_deConf ()=
  deConf.(0) <- read_float ();
  deConf.(1) <- read_float ();
  deConf.(2) <- read_float ()
in

let read_glConf ()=
  (glConf.(0)<-read_int ();glConf.(1)<-read_int ())
in

let setup ()=
(
 read_environment 0;
 read_deConf ();
 read_cMap ();
 read_gene ();
 read_glConf ();
 decode_init ();
)
in
setup();
shooting_sub 0.85932 0.1252 0.2523 10000
