open Out

(* 2段ジャンプと直後へのジャンプを削除するモジュール *)
(* ついでに連続したラベルを一つにまとめる *)
(* ついでに　ラベル+1行のコード+J/Jr/Returnの組み合わせを1つにまとめる *)
(* ついでに絶対にたどり着けないコードを除去 *)

(* ラベル+Returnがあったかどうかのフラグ *)
let lr = ref false
let ret = ref ""

(* 循環書き換えを防止しつつ書き換え情報を追加 *)
let addre f t env = 
  if f = t then env
  else M.add f t (M.map (fun x -> if x = f then t else x) env)

(* mからoの部分列を検索 *)
let findm m o =
  let o' = List.rev o in
  let rec f = function
    | [] -> raise Not_found
    | (Label s)::xs ->
	(try (s, List.assoc (List.filter (function Label _ -> false | _ -> true) xs) m)
	with Not_found -> f xs)
    | _::xs -> f xs in
  f o'

(* ラベルとジャンプ等に囲まれた部分を全てmに追加 *)
let addm m o =
  let rec f m = function
    | [] -> m
    | (Label s)::xs -> f ((xs,s)::m) xs
    | _::xs -> f m xs in
  f m (List.rev o)	  

(* 無駄なコードの並びを探し,それを消したコードと,
   書き換えるべきラベルの集合を返す関数. *)
let rec find env m r o = function
  | [] -> (env, List.rev (o@r))
  | (Label s)::(J x)::xs -> find (addre s x env) m r o (J x::xs)
  | (Label s)::(Label t)::xs -> find (addre s t env) m r o (Label t::xs)
  | (J x | BEq(_,_,x,_) | BLT(_,_,x,_) | BLE(_,_,x,_) | FBEq(_,_,x,_) | FBLT(_,_,x,_) | FBLE(_,_,x,_))::(Label y)::xs when x = y ->
      find env m r o (Label y::xs)
  | (Label s)::Return::xs when not !lr ->
      let l = Id.genid "return" in
      lr := true;
      ret := l;
      find (addre s l env) m r o (Label l::Return::xs)
  | ((Jr _ | J _ | Return) as y)::xs ->
      (try let (s,t) = findm m (y::o) in
          find (addre s t env) m (y::o@r) [] xs
      with Not_found -> find env (addm m (y::o)) (y::o@r) [] xs)
  | (Label u as y)::xs ->
      (try let (s,t) = findm m (J u::o) in
          find (addre s t env) m r (y::o) xs
      with Not_found -> find env (addm m (J u::o)) r (y::o) xs)
  | x::xs -> find env m r (x::o) xs
  

(* コード中のラベルへのジャンプを別のラベルへのジャンプに書き換える関数 *)
and rename env r = function
  | [] -> List.rev r
  | SetL(x, l)::xs when M.mem l env ->
      rename env r (SetL(x, M.find l env)::xs)
  | BEq(x, y, l, ds)::xs when M.mem l env ->
      rename env r (BEq(x, y, M.find l env, ds)::xs)
  | BLT(x, y, l, ds)::xs when M.mem l env ->
      rename env r (BLT(x, y, M.find l env, ds)::xs)
  | BLE(x, y, l, ds)::xs when M.mem l env ->
      rename env r (BLE(x, y, M.find l env, ds)::xs)
  | FBEq(x, y, l, ds)::xs when M.mem l env ->
      rename env r (FBEq(x, y, M.find l env, ds)::xs)
  | FBLT(x, y, l, ds)::xs when M.mem l env ->
      rename env r (FBLT(x, y, M.find l env, ds)::xs)
  | FBLE(x, y, l, ds)::xs when M.mem l env ->
      rename env r (FBLE(x, y, M.find l env, ds)::xs)
  | J l::xs when M.mem l env && M.find l env = !ret ->
      rename env (Return::r) xs 
  | J l::xs when M.mem l env ->
      rename env r (J (M.find l env)::xs)
  | Call l::xs when M.mem l env ->
      rename env r (Call (M.find l env)::xs) 
  | x::xs -> rename env (x::r) xs


(* たどり着けないコードの除去 *)
let rec g ret = function
   | [] -> List.rev ret
   | J l::xs -> g' (J l::ret) xs
   | Jr x::xs -> g' (Jr x::ret) xs
   | Return::xs -> g' (Return::ret) xs
   | Halt::xs -> g' (Halt::ret) xs
   | x::xs -> g (x::ret) xs
(* ここはたどり着けない *)
and g' ret = function
   | [] -> List.rev ret
   | Label l::xs -> g (Label l::ret) xs
   | Comment s::xs -> g (Comment s::ret) xs
   | _::xs -> g' ret xs

(* 無駄なラベルを削除する関数 *)
let rec h'' r s = function
  | [] -> List.rev r
  | Label l::xs when not (S.mem l s) && String.contains l '.' -> h'' r s xs
  | x::xs -> h'' (x::r) s xs
let rec h' s = function
  | [] -> s
  | (SetL(_,l) | BEq(_,_,l,_) | BLE(_,_,l,_) | BLT(_,_,l,_) | FBEq(_,_,l,_) | FBLE(_,_,l,_)  | FBLT(_,_,l,_) | J l | Call l)::xs  -> h' (S.add l s) xs
  | _::xs -> h' s xs
let h all = h'' [] (h' S.empty all) all
    

(* 本体 *)
let f code =
  Format.eprintf "eliminating jumps ...@.";
  let rec f' code =
    let (env, code1) = find M.empty [] [] [] code in
    let code2 = rename env [] code1 in
    let code3 = h code2 in
    let code4 = g [] code3 in
    if code4 = code then code4 else f' code4 in
  f' code

