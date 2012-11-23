(*INPUT
500 400 1 0 0 1.37083 1.0375 1 0.25 0.01
*)
let conf              = Array.create 10 (0.0) in
let bound             = Array.create 4 (0.0) in
let dummy             = Array.create 4 0.0 in
let tbl               = ref (Array.create 1 dummy) in
let deTbl             = ref (Array.create 0 (Array.create 0 0.)) in

let array_gen x=
 Array.make 4 0.0
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

let calcAlpha dnorm gamma linrange=
  if dnorm < linrange then
     (let funcval=linrange ** gamma in
      let frac=dnorm/.linrange in
       (1.0-.frac)*.dnorm*.funcval/.linrange +. frac*.(dnorm**gamma))
  else (dnorm ** gamma)
in

let calcNewColor r g b ls=
  let co=ls/.255.0 in
  (r*.co,g*.co,b*.co)
in

let calcC v alpha=
  if alpha>0.0 then
    (let tmp=v/.alpha in
     let tmp2=if tmp>255.0 then 255.0 else if tmp<0.0 then 0.0 else tmp in
     int_of_float (tmp2*.alpha))
  else 0
in

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
in

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
  print_char 'P'; print_char '3'; print_char '\n';
  print_int (int_of_float ret_width); print_char ' ';
  print_int (int_of_float ret_height); print_char ' '; print_int 255; print_char '\n';
printer 0
)
in
read_environment 0;
output_ppm 0
