open Out

(* 遅延スロットをうめるモジュール *)


(* その命令で使われるレジスタ *)
let used = function
  | Label _ | SetL _ | Nop | Comment _ | FMvlo _ | FMvhi _ | J _ | Call _ | Return | Inputb _ | Halt -> S.empty
  | AddI(_,x,_) | SubI(_,x,_) | XorI(_,x,_) | SllI(_,x,_) | SraI(_,x,_) | IMovF(_,x) | FMovI(_,x) | FInv(_,x) | FSqrt(_,x) | LdI(_,x,_) | FLdI(_,x,_) | Jr(x) | CallR(x) | Outputb(x) -> S.singleton x
  | Add(_,x,y) | Sub(_,x,y) | Xor(_,x,y) | FAdd(_,x,y) | FSub(_,x,y) | FMul(_,x,y) | StI(x,y,_) | LdR(_,x,y) | FStI(x,y,_) | FLdR(_,x,y) | BEq(x,y,_,_) | BLT(x,y,_,_) | BLE(x,y,_,_) | FBEq(x,y,_,_) | FBLT(x,y,_,_) | FBLE(x,y,_,_) -> S.add x (S.singleton y)

(* その命令のターゲットレジスタ *)
let target = function
  | SetL(x,_) | Add(x,_,_) | Sub(x,_,_) | Xor(x,_,_) | AddI(x,_,_) | SubI(x,_,_) | XorI(x,_,_) | FMvlo(x,_) | FMvhi(x,_) | SllI(x,_,_) | SraI(x,_,_) | IMovF(x,_) | FMovI(x,_) | FAdd(x,_,_) | FSub(x,_,_) | FMul(x,_,_) | FInv(x,_) | FSqrt(x,_) | LdI(x,_,_) | LdR(x,_,_) | FLdI(x,_,_) | FLdR(x,_,_) | Inputb(x) -> Some x
  | _ -> None


(* 遅延スロットに入れても大丈夫な命令を集める
   readは分岐命令に影響するレジスタ。
   writeは分岐命令までの間に上書きされるレジスタ *)
let rec fill n read write l =
  if n <= 0 then ([], l) else
  match l with
  | [] -> ([], [])
  | (Comment _ as i)::xs -> let (ds, xs') = fill n read write xs in (ds, i::xs')
  | (Label _ | BEq _ | BLT _ | BLE _ | FBEq _ | FBLT _ | FBLE _ | J _ | Jr _ | Call _ | CallR _ | Return | Halt)::_ -> ([], l) 
  | i::xs ->
      let u = used i in
      match target i with
      | Some x ->
	  if not (S.mem x read) && S.is_empty (S.inter write u) then
	    let (ds, xs') = fill (n-1) read write xs in
            (i::ds, xs')
	  else
	    let (ds, xs') = fill n (S.union u read) (S.add x write) xs in
	    (ds, i::xs')
      | None ->
	  if S.is_empty (S.inter write u) then
	    let (ds, xs') = fill (n-1) read write xs in
            (i::ds, xs')
	  else
	    let (ds, xs') = fill n (S.union u read) write xs in
	    (ds, i::xs')



(* 本体 *)
let rec g ret = function
  | [] -> ret
  | BEq (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (BEq(x, y, l, List.rev ds)::ret) xs'
  | BLT (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (BLT(x, y, l, List.rev ds)::ret) xs'
  | BLE (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (BLE(x, y, l, List.rev ds)::ret) xs'
  | FBEq (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (FBEq(x, y, l, List.rev ds)::ret) xs'
  | FBLT (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (FBLT(x, y, l, List.rev ds)::ret) xs'
  | FBLE (x, y, l, ds)::xs ->
      assert (List.length ds = 0);
      let (ds, xs') = fill dsn (S.of_list [x;y]) S.empty xs in
      g (FBLE(x, y, l, List.rev ds)::ret) xs'
  | x::xs -> g (x::ret) xs


let f e = g [] (List.rev e)
