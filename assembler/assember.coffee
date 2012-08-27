#!/usr/bin/env node


fs = require 'fs'
encoding = 'utf-8'

splitLines = (file) ->
  file.split("\n").map((line) ->
    line.replace(/;.*/, '').trim()).filter (line) ->
      line != ''


extractLabels = (lines) ->
  labels = {}
  for i of lines
    if lines[i][0] == '.'
      label = lines[i].substr(1)
      labels[label] = i
  labels


instCode = (inst) ->
  switch inst
    when 'and'  then '000000'
    when 'or'   then '000001'
    when 'add'  then '000010'
    when 'sub'  then '000110'
    when 'slt'  then '000111'

    when 'andi' then '001000'
    when 'ori'  then '001001'
    when 'addi' then '001010'
    when 'subi' then '001110'
    when 'slti' then '001111'

    when 'out'  then '000100'
    when 'in'   then '001100'

    when 'lw'   then '100011'
    when 'sw'   then '101011'

    when 'beq'  then '111110'
    when 'j'    then '111111'


String::repeat = (num) ->
  buf = ""
  buf += this for i in [0..num]
  buf

rjust = (str, width) ->
  padding = "0";
  if( str.length < width )
    padding.repeat( width - 1 - str.length ) + str
  else
    str

String::toBin = (width) ->
  parseInt(@).toBin(width)

Number::toBin = (width) ->
  b = @
  r = []
  for i in [0..width-1]
    r.unshift b & 1
    b = b >> 1
  r.join('')


toInstruction = (line, line_no, labels) ->
  reg = (x) ->
    x.match(/\$(\d+)/)[1].toBin(5)
  imm = (x) ->
    x.toBin(16)

  label_abs = (labels, label) ->
    labels[label]

  label_relative = (labels, label, current) ->
    labels[label] - current - 1

  return '' if line[0] == '.'

  inst = line.split(' ')[0]
  args = line.substr(inst.length).split(",").map (w) -> w.trim()
  switch line.split(' ')[0]
    when 'andi', 'ori', 'addi', 'subi', 'slti'
      instCode(inst) + reg(args[1]) + reg(args[0]) + imm(args[2])
    when 'and', 'or', 'add', 'sub', 'slt'
      instCode(inst) + reg(args[1]) + reg(args[2]) + reg(args[0]) + '00000000000'
    when 'sw', 'lw'
      [_, relative, pointer_reg] = args[1].match /(\d*)\((.*)\)/
      instCode(inst) + reg(pointer_reg) + reg(args[0]) + imm(relative)
    when 'in', 'out'
      instCode(inst) + "00000" + reg(args[0]) + imm('0')
    when 'j'
      instCode(inst) + label_abs(labels, args[0]).toBin(26)
    when 'beq'
      instCode(inst) + reg(args[0]) + reg(args[1]) + label_relative(labels, args[2], line_no).toBin(16)
    when 'nop'
      '0'.toBin(32)

contents = fs.readFileSync("/dev/stdin", encoding) # TODO: is this cross-platform?
lines = splitLines(contents)
console.dir lines
labels = extractLabels(lines)
console.dir labels
for i of lines
  code = toInstruction(lines[i], i, labels)
  console.log(rjust(parseInt(code, 2).toString(16), 8)) unless code == ''
