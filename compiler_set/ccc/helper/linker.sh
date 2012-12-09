#!/bin/sh
sed "/asm_here:/ {
    r `dirname $0`/assembly.s
    d
}" $1
