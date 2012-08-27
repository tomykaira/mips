;; instruction for df23d96
    addi $2, $0, 5
    addi $3, $0, 12
    addi $7, $3, -9
    or   $4, $7, $2
    and  $5, $3, $4
    add  $5, $5, $4
    beq  $5, $7, end
    slt  $4, $3, $4
    beq  $4, $0, around
    addi $5, $0, 0
.around
    slt  $4, $7, $2  
    add  $7, $4, $5
    sub  $7, $7, $5
    out  $7                     ; expects: 7
    sw   $7, 68($3)
    lw   $2, 80($0)
    j    end
    addi $2, $0, 1
.end
    in   $5
    out  $5                     ; expects: input value(105 = x69 in test)
    sw   $5, 76($3)
    lw   $2, 80($0)
    sw   $2, 84($0)
    out  $2                     ; expects: 7
    out  $0                     ; test done flag
    nop
    in   $0                     ; BLOCK
