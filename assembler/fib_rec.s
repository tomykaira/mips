        in $5

        subi $31, $31, 4
        savepc 0($31)
        j rec
        
        out $1
        j end
.rec
        beq $5, $0, ret1
        subi $5, $5, 1
        beq $5, $0, ret1

        subi $31, $31, 4
        sw $5, 0($31)

        subi $31, $31, 4
        savepc 0($31)
        j rec

        subi $31, $31, 4
        sw $1, 0($31)

        lw $5, 4($31)
        subi $5, $5, 1

        subi $31, $31, 4
        savepc 0($31)
        j rec

        lw $2, 0($31)
        add $1, $1, $2

        addi $31, $31, 8
        j ret

.ret1
        addi $1, $0, 1

.ret
        out $1
        lw $30, 0($31)
        addi $31, $31, 4
        jr $30

.end
        out $0
