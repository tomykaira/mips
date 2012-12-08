open ANormal

(* エイリアス解析と、それを用いた最適化を行うモジュール *)

(* 解析結果のグラフ。 *)
let graph = ref (Type.t * M.empty) list




let f e =
  let g = analyse
