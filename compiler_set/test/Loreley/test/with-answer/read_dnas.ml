(*INPUT
0 0.5 0.5 5
-0.0557357 0.296215 0.345357 0.819319 0.740205 -0.591994
-0.212083 0.653373 -0.458351 0.589448 -0.535298 0.526096
1 2 0.349591
1 3 0.286532
1 4 0.0100913
3 5 0.0804772 -0.265128 -0.613688
1 6 0.273309
*)
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
(* test functions *)
let print_list l =
  let (fn, params) = l in
  print_int fn;
  if fn = 5 then
    let p1 :: p2 :: p3 :: empty = params in
    (print_int5 p1; print_int5 p2; print_int5 p3)
  else
    let p1 :: empty = params in
    print_int5 p1
in
let print_dna result = 
let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),data,(d1,d2,d3,d4,d5,d6)) = result in
print_int5 cIndex;
print_int5 cBlendRate; 
print_int5 cWeight;
print_int5 c1;
print_int5 c2;
print_int5 c3;
print_int5 c4;
print_int5 c5;
print_int5 c6;
print_int5 d1;
print_int5 d2;
print_int5 d3;
print_int5 d4;
print_int5 d5;
print_int5 d6;
let l1 :: l2 :: l3 :: l4 :: l5 :: empty = data in
print_list l1;
print_list l2;
print_list l3;
print_list l4;
print_list l5;
if empty == [] then print_char 'O' else print_char 'X';
print_char '\n'
in
print_dna (read_dnas 0)
