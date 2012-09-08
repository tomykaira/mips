
val fmul = _import "fmul" : (real, real) -> real

(* http://www.mlton.org/InfixingOperators *)
infix  3 <\     fun x <\ f = fn y => f (x, y)     (* Left section      *)
infix  3 \>     fun f \> y = f y                  (* Left application  *)
infixr 3 />     fun f /> y = fn x => f (x, y)     (* Right section     *)
infixr 3 </     fun x </ f = f x                  (* Right application *)


fun smlMul (a,b) = a * b

fun max a b = if a > b then a else b

fun assertEqual (a, b) =
    let
        fun pow2 1 = 2.0
          | pow2 n = pow2 (n div 2) *  pow2 (n - n div 2)
        val mine = fmul(a, b)
        val sml = smlMul(a, b)
        val epsilon = 1.0 / (pow2 126)
    in
    if abs(mine - sml) <= max (sml / pow2 22) epsilon then
        print "."
    else
        print ("\n" ^ Real.toString a ^ " + " ^ Real.toString b ^ ":\n  Expected: " ^ Real.toString sml ^ "\n  Actual: " ^ Real.toString mine)
    end

val _ =
    let
        val input = TextIO.inputAll TextIO.stdIn
        val testcases = map (String.tokens (op= /> #",")) (String.tokens (op= /> #"\n")input)
    in
        map (fn a :: b :: [] => assertEqual (valOf (Real.fromString a), valOf (Real.fromString b)))
    end
