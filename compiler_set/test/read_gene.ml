(* input:
0.5 0.5 5
-0.848127 0.670267 0.618477 0.456033 -0.112178 0.36084
-0.599703 -0.815963 -0.293803 -0.780384 0.924242 0.114203
1 2 0.349591
1 3 0.286532
1 4 0.0100913
3 5 0.0804772 0.0112989 0.866422
1 6 0.273309
 *)
(*  let buf = Buffer.create 16 *)
(*  in *)

(*  let rec read_token in_token = *)
(*    try *)
(*      let c = input_char stdin in *)
(*      match c with *)
(*        ' ' | '\t' | '\r' | '\n' -> *)
(*  	if in_token then () *)
(*  	else read_token false *)
(*      | _ -> *)
(*  	Buffer.add_char buf c; *)
(*  	read_token true *)
(*    with *)
(*      End_of_file -> *)
(*        if in_token then () else raise End_of_file *)
(*  in *)

(*  let read_float () = *)
(*    Buffer.clear buf; *)
(*    read_token false; *)
(*    try *)
(*      float_of_string (Buffer.contents buf) *)
(*    with *)
(*      Failure _ -> failwith ((Buffer.contents buf) ^ ": float conversion failed.") *)
(* in *)
(*  let read_int () = *)
(*    Buffer.clear buf; *)
(*    read_token false; *)
(*    try *)
(*      int_of_string (Buffer.contents buf) *)
(*    with *)
(*      Failure _ -> failwith ((Buffer.contents buf) ^ ": int conversion failed.") *)
(*  in *)

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

let read_dnas _=
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
let print_float5 x = print_int(int_of_float (x *. 10000.0)); print_char '\n' in
let dnaArray          = ref (Array.create 0 (0., 0., 0., (0., 0., 0., 0., 0., 0.), [], (0., 0., 0., 0., 0., 0.))) in
let read_gene ()=
  let genes = read_int() in
  let _ = read_int() in
  dnaArray:=Array.init genes read_dnas;
in
read_gene ();
let print_nth n = 
let (cIndex,cBlendRate,cWeight,(c1,c2,c3,c4,c5,c6),data,(d1,d2,d3,d4,d5,d6)) = !dnaArray.(n) in
print_float5 cIndex;
print_float5 cBlendRate; 
print_float5 cWeight;
print_float5 c1;
print_float5 c2;
print_float5 c3;
print_float5 c4;
print_float5 c5;
print_float5 c6;
print_float5 d1;
print_float5 d2;
print_float5 d3;
print_float5 d4;
print_float5 d5;
print_float5 d6
in
let rec itete n = if n < 21 then (print_nth n; itete (n + 1)) else () in
itete 0
