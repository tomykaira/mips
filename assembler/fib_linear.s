.main
        addi $1, $0, 0
        addi $2, $0, 1
        in $5
.loop
        add $3, $1, $2
        add $1, $2, $0
        add $2, $3, $0
        out $3
        subi $5, $5, 1

        beq $5, $0, end
        j loop
.end
        out $0
