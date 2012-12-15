#!/bin/sh
sed $2 "/asm_here:/ {
    r `dirname $0`/assembly.s
    d
}" $1
