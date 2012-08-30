#!/usr/bin/env coffee
# usage: ./asm_min.coffee < input.s

fs = require 'fs'
util = require 'util'
encoding = 'utf-8'

instruction_length = if process.argv.length > 2 then process.argv[process.argv.length-1] else 64

splitParts = (file) ->
  file.split("\n\n")


removeLabels = (lines) ->
  labels = {}
  i = 0
  while i < lines.length
    if lines[i][0] == '.'
      label = lines.splice(i, 1)[0].substr(1)
      labels[label] = i
    else
      i += 1
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

    when 'SNDB'  then '000100'
    when 'RBYT'   then '001100'

    when 'lw'   then '100011'
    when 'sw'   then '101011'
    when 'savepc' then '101100'

    when 'jr'   then '111101'
    when 'beq'  then '111110'
    when 'JMP'    then '111111'
    else
      throw "Unknown instruction #{inst}"


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
  int = parseInt(@)
  if isNaN(int)
    throw "Cannot parse #{@} to int"
  else
    int.toBin(width)

Number::toBin = (width) ->
  b = @
  r = []
  for i in [0..width-1]
    r.unshift b & 1
    b = b >> 1
  r.join('')

reg = (x) ->
  match = x.match(/\%r(\d+)/)
  if match
    match[1].toBin(5)
  else
    throw "Register #{x} does not match $(\d+)"

imm = (x) ->
  x.toBin(16)


toInstruction = (line, labels) ->

  match = line.match(/; pc = (\d+) ; ([A-Z]*) ([^;]*);/)

  raise "#{line} is malformed" unless match

  pc   = match[1]
  inst = match[2]
  args = match[3].split(" ").map (x) -> x.trim()

  label_abs = (labels, label) ->
    labels[label]

  label_relative = (labels, label, current) ->
    labels[label] - current - 1

  switch inst
    when 'andi', 'ori', 'addi', 'subi', 'slti'
      instCode(inst) + reg(args[1]) + reg(args[0]) + imm(args[2])
    when 'and', 'or', 'add', 'sub', 'slt'
      instCode(inst) + reg(args[1]) + reg(args[2]) + reg(args[0]) + '00000000000'
    when 'sw', 'lw'
      [_, relative, pointer_reg] = args[1].match /(\d*)\((.*)\)/
      instCode(inst) + reg(pointer_reg) + reg(args[0]) + imm(relative)
    when 'savepc'
      [_, relative, pointer_reg] = args[0].match /(\d*)\((.*)\)/
      instCode(inst) + reg(pointer_reg) + "00000" + imm(relative)
    when 'RBYT', 'SNDB'
      instCode(inst) + "00000" + reg(args[0]) + imm('0')
    when 'JMP'
      instCode(inst) + label_abs(labels, args[0]).toBin(26)
    when ''
      instCode(inst) + "00000" + reg(args[0]) + imm('0')
    when 'beq'
      instCode(inst) + reg(args[0]) + reg(args[1]) + label_relative(labels, args[2], line_no).toBin(16)
    when 'nop'
      '0'.toBin(32)
    else
      throw "Unknown instruction #{inst}"

toMap = (constants) ->
  constants.split("\n").reduce (s, x) ->
    match = x.match(/"([^"]*)" = (\d+)/)
    if match
      s[match[1]] = parseInt(match[2])
    s
  , {}



contents = fs.readFileSync("/dev/stdin", encoding) # TODO: is this cross-platform?
instructions, constants = splitParts(contents)
constant_map = toMap(constants)
lines = instructions.split("\n").map (i) -> toInstruction i constant_map
for i of [0..lines.length]
  if lines[i]
    console.log lines[i]
  else
    console.log ''
util.error("Instruction length: #{lines.length}")
