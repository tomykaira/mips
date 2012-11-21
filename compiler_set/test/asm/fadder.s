  j min_caml_start
min_caml_start:
  fmvhi $f3, 16384
  fadd $f4, $f3, $f0
  fadd $f5, $f4, $f3
  fmul $f7, $f5, $f3
  nop
  nop
  nop
  fmul $f8, $f7, $f7
  fsqrt $f9, $f8
