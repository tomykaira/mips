let x = Array.create 4 0 in
x.(0)<-1;
x.(1)<-2;
x.(2)<-3;
x.(3)<-4;
let [|a; b; c; d|] = x in
print_int a; print_int b; print_int c; print_int d
