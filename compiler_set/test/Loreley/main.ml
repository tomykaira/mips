
(***** Loreley ver0.1(2012/11/01) *****)

(**************** ここから、関数の定義の変更ok *******************)

(*乱数生成機：xorshift。本来32bit用なので31bitのOcamlでは動作が怪しい *)
let xorX = ref 123456789 in
let xorY = ref 362436069 in
let xorZ = ref 521288629 in
let xorW = ref 88675123  in

let xor128 () =
  let t = !xorX lxor (!xorX lsl 11) in
  (xorX := !xorY; xorY := !xorZ; xorZ := !xorW;
  xorW := (!xorW lxor (!xorW lsr 19)) lxor (t lxor (t lsr 8));
  !xorW)
in

let random_float max=
  (float_of_int (xor128 () land 0x7fffff)) /. (float_of_int 0x7fffff) *. max
in

let random_int max=
  ((xor128 () land 0x7fffffff) mod max)
in


(* 読み込み関数。レイトレと同じ。mincamlでは変更必須 *)
let buf = Buffer.create 16
in

let rec read_token in_token =
  try
    let c = input_char stdin in
    match c with
      ' ' | '\t' | '\r' | '\n' ->
	if in_token then ()
	else read_token false
    | _ ->
	Buffer.add_char buf c;
	read_token true
  with
    End_of_file ->
      if in_token then () else raise End_of_file

let read_float () = 
  Buffer.clear buf;
  read_token false;
  try
    float_of_string (Buffer.contents buf)
  with
    Failure _ -> failwith ((Buffer.contents buf) ^ ": float conversion failed.")

let read_int () = 
  Buffer.clear buf;
  read_token false;
  try
    int_of_string (Buffer.contents buf)
  with
    Failure _ -> failwith ((Buffer.contents buf) ^ ": int conversion failed.")


(* グローバルな変数。一部の領域は設定ファイルを読み込みながら動的に確保される。Array.make、Array.initはmincamlにおいて変更が必要 *)
let conf=Array.make 10 (0.0)
let bound=Array.make 4 (0.0)
let tbl=ref (Array.make 1 [|0.0;0.0;0.0;0.0|])
let cMap=ref (Array.make 1 (0.0,0.0,0.0))
let cConf=Array.make 1 256
let deConf=Array.make 3 0.0
let dnaArray=ref [||]
let finalXform=ref (0.0,0.0,0.0,(0.0,0.0,0.0,0.0,0.0,0.0),[],(0.0,0.0,0.0,0.0,0.0,0.0))
let genes=ref 0
let enable_finalXform=ref 0
let batch_ct=10000
let deCoeffs=ref [||]
let filterWidth=ref [||]
let deTbl=ref [||]
let funcRandTblSize=1024
let funcRandTbl=Array.make funcRandTblSize 0
let glConf=Array.make 2 0

(**************** ここまで、関数の定義の変更ok *******************)


(* 以下は計算用ロジック *)
let rec gene_gen res=
  if res==0 then []
  else 
  (
   let head=read_float () in
   head::(gene_gen (res - 1))
  )

let rec dna_gen res=
  if res==0 then []
  else
  (
  let len=read_int () in
  let fType=read_int () in
  let head=(fType,gene_gen len) in
  head::(dna_gen (res - 1))
  )

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

let read_gene ()=
(genes := read_int();enable_finalXform:=read_int();
dnaArray:=Array.init (!genes) read_dnas;
if !enable_finalXform ==1 then (finalXform:=read_dnas 0) else ())

let rec read_cMap_sub iter=
(
 let r=read_float () in
 let g=read_float () in
 let b=read_float () in
 !cMap.(iter) <- (r,g,b);
 if iter==(cConf.(0)-1) then ()
 else read_cMap_sub (iter + 1)
)

let read_cMap ()=
(
 cConf.(0) <- read_int ();
 cMap:=Array.make cConf.(0) (0.0,0.0,0.0);
 read_cMap_sub 0
)

let array_gen x=
 Array.make 4 0.0

let decode_init ()=
  let rec acmWeight pos min sum=
    if pos==(!genes) then (min,sum) else
    let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !dnaArray.(pos) in
    acmWeight (pos+1) (if min>cWeight then cWeight else min) (sum+.cWeight)
  in
  let min,sum=acmWeight 0 1000000.0 0.0 in
  (let rec initRandTbl_sub v pos rest=
     if rest==0 then pos else
     (funcRandTbl.(pos)<-v;initRandTbl_sub v (pos+1) (rest-1))
   in
   let rec initRandTbl fn index=
     if fn==(!genes) then () else
     (let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !dnaArray.(fn) in
     initRandTbl (fn+1) (initRandTbl_sub fn index (int_of_float (cWeight*.(float_of_int funcRandTblSize)/.sum)))) in
   initRandTbl 0 0)

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

let apply_func (fn,coeffs) x y=
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
   let tmpr=(2.0*.3.14159265*.t_rnd+.(if (int_of_float t_rnd) land 1==0 then (atan2 y x) else 0.0-.(atan2 y x)))/.power in
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

let rec apply_gene_sub fn_ary x y retx rety=
  if fn_ary==[] then (retx,rety)
  else
    (
     let head::tail=fn_ary in
       (let (nx,ny)=apply_func head x y in
       apply_gene_sub tail x y (retx+.nx) (rety+.ny))
    )

let apply_gene fn x y col=
  let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !dnaArray.(fn) in
  let nx,ny=apply_gene_sub fun_ary (c1*.x+.c3*.y+.c5) (c2*.x+.c4*.y+.c6) 0.0 0.0 in
  ((d1*.nx+.d3*.ny+.d5),(d2*.nx+.d4*.ny+.d6),(col *. (1.0 -. cBlendRate)) +. cIndex*.cBlendRate)

let apply_finalXform x y col=
let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),fun_ary,(d1,d2,d3,d4,d5,d6))= !finalXform in
  let nx,ny=apply_gene_sub fun_ary (c1*.x+.c3*.y+.c5) (c2*.x+.c4*.y+.c6) 0.0 0.0 in
  ((d1*.nx+.d3*.ny+.d5),(d2*.nx+.d4*.ny+.d6),(col *. (1.0 -. cBlendRate)) +. cIndex*.cBlendRate)

(* 正則性判定 *)
let regular x y=
  not ((x<bound.(0)) or (x>bound.(1)) or (y<bound.(2)) or (y>bound.(3)))

let random_gen ()=
(random_float 2.0) -. 1.0

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
     if iter==0 then () else shooting_sub nx ny ncol (iter - 1))
 else
    (if iter==0 then () else shooting_sub nx ny ncol iter)
)

let rec shooting iter=
(
 shooting_sub (random_gen ()) (random_gen ()) (random_float 1.0) batch_ct;
 if iter<=0 then ()
 else shooting (iter - batch_ct)
)

let read_deConf ()=
  deConf.(0) <- read_float ();
  deConf.(1) <- read_float ();
  deConf.(2) <- read_float ()

let gauss rad=
  (exp (-2.0*.rad*.rad)) *. (sqrt (2.0/.3.14159265))

let create_deFilter ()=
  (let max_rad=deConf.(0)*.conf.(2) in
  let min_rad=deConf.(1)*.conf.(2) in
  let kernel_size=int_of_float ((max_rad+.1.0)*.(max_rad+.2.0)/.2.0) in
  let oversample=int_of_float conf.(2) in
  let ov2=int_of_float (floor (conf.(2)/.2.0)) in
  let curve=deConf.(2) in
  let max_index_count=128 in
  let width_threshold=100 in
  deCoeffs:=Array.make (kernel_size*max_index_count) 0.0;
  filterWidth:=Array.make max_index_count 0;
  let deWidth pos=
    (if pos>=width_threshold then 
      (max_rad/. (((float_of_int width_threshold)**curve) +. ((float_of_int (pos+1-width_threshold))**(1.0/.curve)) ) )
    else
      (max_rad/.((float_of_int (pos+1))**curve)))
  in
  let rec deFilter_sub pos=
     if pos==max_index_count then ()
     else (
        !filterWidth.(pos) <- 
        int_of_float (ceil (deWidth pos));
        deFilter_sub (pos+1)
      )
  in
  let rec createFilter pos cIndex=
    let rec sumGauss x y sum=
       if x>max_rad then
        (if y>max_rad then sum
         else sumGauss (0.0-.max_rad) (y+.1.0) sum)
       else 
          (let tmp=(sqrt (x*.x+.y*.y))/.(deWidth pos) in
           sumGauss (x+.1.0) y (if tmp<=1.0 then sum+.(gauss tmp) else sum))
    in
    let rec setDeCoeffs x y index sumWeight=
       if y<=x then
          (let tmp=(sqrt (x*.x+.y*.y))/.(deWidth pos) in
           if tmp<=1.0 then !deCoeffs.(index) <- (gauss tmp)/.sumWeight
           else !deCoeffs.(index) <- 0.0;
           setDeCoeffs x (y+.1.0) (index+1) sumWeight)
       else
         (if x<max_rad then setDeCoeffs (x+.1.0) 0.0 index sumWeight else index)
    in
    if pos<max_index_count then
      createFilter (pos+1) (setDeCoeffs 0.0 0.0 cIndex (sumGauss (1.0-.max_rad) (1.0-.max_rad) 0.0))
    else ()
  in
  deFilter_sub 0;createFilter 0 0)

let de_filter ()=
  (create_deFilter ();
  let ret_width=int_of_float (conf.(0)+.conf.(2)*.6.0) in
  let ret_height=int_of_float (conf.(1)+.conf.(2)*.6.0) in
let rec apply_deFilter ()=
  let max_rad=deConf.(0)*.conf.(2) in
  let min_rad=deConf.(1)*.conf.(2) in
  let kernel_size=int_of_float ((max_rad+.1.0)*.(max_rad+.2.0)/.2.0) in
  let oversample=int_of_float conf.(2) in
  let ov2=int_of_float (floor (conf.(2)/.2.0)) in
  let curve=deConf.(2) in
  let max_index_count=128 in
  let width_threshold=100 in
  let rec apply_de_sub i j=
    (if i>=ret_width-ov2-1 then (if j<ret_height-ov2-1 then apply_de_sub ov2 (j+1) else ())
    else
    (let rec summ p q sum=
    if q<=ov2 then summ p (q+1) (sum+.(!tbl.((i+p)+(j+q)*ret_width).(3)))
    else (if p<=ov2 then summ (p+1) (-ov2) sum else sum) in
    let sum=(summ (-ov2) (-ov2) 0.0)/.255.0 in
    let cIndex=if (int_of_float sum)>=max_index_count then max_index_count-1 else (int_of_float sum) in
    let de_width= !filterWidth.(cIndex) in
    let tindex=kernel_size*cIndex in
    let emit_col r g b dense x y=
      if x>=0 && y>=0 && x<=ret_width && y<=ret_height then 
      (let det= !deTbl.(x+y*ret_width) in
       det.(0)<-det.(0)+.r;
       det.(1)<-det.(1)+.g;
       det.(2)<-det.(2)+.b;
       det.(3)<-det.(3)+.dense) else ()
    in
    let rec emit p q index=
      if q<=p then
        (let [|r;b;g;dense|]= !tbl.(i+j*ret_width) in
         let nr,nb,ng,ndense=
             r*. !deCoeffs.(index),g*. !deCoeffs.(index),
             b*. !deCoeffs.(index),dense*. !deCoeffs.(index) in
             if p==0 && q==0 then
               (emit_col nr ng nb ndense i j)
             else if q==0 then
               (emit_col nr ng nb ndense (i+p) (j);
                emit_col nr ng nb ndense (i-p) (j);
                emit_col nr ng nb ndense (i) (j+p);
                emit_col nr ng nb ndense (i) (j-p))
             else if p==q then
               (emit_col nr ng nb ndense (i+p) (j+p);
                emit_col nr ng nb ndense (i+p) (j-p);
                emit_col nr ng nb ndense (i-p) (j+p);
                emit_col nr ng nb ndense (i-p) (j-p))
             else
               (emit_col nr ng nb ndense (i+p) (j+q);
                emit_col nr ng nb ndense (i+p) (j-q);
                emit_col nr ng nb ndense (i-p) (j+q);
                emit_col nr ng nb ndense (i-p) (j-q);
                emit_col nr ng nb ndense (i+q) (j+p);
                emit_col nr ng nb ndense (i+q) (j-p);
                emit_col nr ng nb ndense (i-q) (j+p);
                emit_col nr ng nb ndense (i-q) (j-p));
                emit p (q+1) (index+1)
           )
      else (if p<=de_width then (emit (p+1) (-ov2) index) else ()) in
    (emit 0 0 tindex;apply_de_sub (i+1) j)))
  in
  apply_de_sub ov2 ov2
  in
  deTbl:=Array.init ((ret_width+1)*(ret_height+1)) array_gen;
  apply_deFilter ())

let calcAlpha dnorm gamma linrange=
  if dnorm < linrange then
     (let funcval=linrange ** gamma in
      let frac=dnorm/.linrange in
       (1.0-.frac)*.dnorm*.funcval/.linrange +. frac*.(dnorm**gamma))
  else (dnorm ** gamma)

let calcNewColor r g b ls=
  let co=ls/.255.0 in
  (r*.co,g*.co,b*.co)

let calcC v alpha=
  if alpha>0.0 then
    (let tmp=v/.alpha in
     let tmp2=if tmp>255.0 then 255.0 else if tmp<0.0 then 0.0 else tmp in
     int_of_float (tmp2*.alpha))
  else 0

let calcRGBA ir ig ib idense=
  let ls=if idense==0.0 then 0.0 else (1067.8*.(log (1.0+.idense*.7.352941176e-5))/.idense) in
  let r,g,b,dense=ir*.ls,ig*.ls,ib*.ls,idense*.ls in
  let tdense=dense/.255.0 in
  let gamma,linrange=conf.(8),conf.(9) in
  let alpha,nls=if dense<=0.0 then 0.0,0.0 else
    (let talpha=calcAlpha tdense gamma linrange in
    (if talpha<0.0 then 0.0 else 
    if talpha>1.0 then 1.0 else talpha),256.0*.talpha/.tdense) in
  let nr,ng,nb=calcNewColor r g b nls in
  (calcC nr alpha,calcC ng alpha,calcC nb alpha)

let output_ppm mode=
let ret_width=(conf.(0)+.conf.(2)*.6.0) in
let ret_height=(conf.(1)+.conf.(2)*.6.0) in
let rec printer n=
if n==(int_of_float (ret_width*.ret_height)) then ()
else
 (let [|oa;ob;oc;od|]= (if mode==1 then !deTbl.(n) else !tbl.(n)) in
  let a,b,c=calcRGBA oa ob oc od in
  print_int a;print_char ' ';
  print_int b;print_char ' ';
  print_int c;print_char ' ';
  print_char '\n';
  printer (n+1))
in
(
print_string "P3
";print_int (int_of_float ret_width);print_char ' ';
print_int (int_of_float ret_height);print_string " 255
";
printer 0
)

let read_glConf ()=
  (glConf.(0)<-read_int ();glConf.(1)<-read_int ())

let flam ()=
(
 read_environment 0;
 read_deConf ();
 read_cMap ();
 read_gene ();
 read_glConf ();
 decode_init ();
 shooting glConf.(0);
 if glConf.(1)==1 then (de_filter ();output_ppm 1) else (output_ppm 0)
)

let _ = flam ()
