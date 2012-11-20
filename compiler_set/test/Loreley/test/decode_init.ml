(*INPUT
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
*)
let dnaArray          = ref (Array.create 0 (0., 0., 0., (0., 0., 0., 0., 0., 0.), [], (0., 0., 0., 0., 0., 0.))) in
let genes             = ref 0 in
let funcRandTblSize   = 1024 in
let funcRandTbl       = Array.create funcRandTblSize 0 in
let enable_finalXform = ref 0 in
let finalXform        = ref (0.0,0.0,0.0,(0.0,0.0,0.0,0.0,0.0,0.0),[],(0.0,0.0,0.0,0.0,0.0,0.0)) in
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
let decode_init ()=
  let rec acmWeight pos min sum=
    if pos==(!genes) then (min,sum) else
    let (cIndex,cBlendRate,cWeight,_,fun_ary,_)= !dnaArray.(pos) in
    acmWeight (pos+1) (if min>cWeight then cWeight else min) (sum+.cWeight)
  in
  let (min, sum) = acmWeight 0 1000000.0 0.0 in
  (let rec initRandTbl_sub v pos rest=
     print_int pos; print_char ' '; print_int v; print_char '\n'; 
     if rest==0 then pos else
     (funcRandTbl.(pos)<-v;initRandTbl_sub v (pos+1) (rest-1))
   in
   let rec initRandTbl fn index=
     if fn==(!genes) then () else
     (let (cIndex,cBlendRate,cWeight,_,fun_ary,_)= !dnaArray.(fn) in
     initRandTbl (fn+1) (initRandTbl_sub fn index (int_of_float (cWeight*.(float_of_int funcRandTblSize)/.sum)))) in
   initRandTbl 0 0)
in
read_gene();
decode_init()
