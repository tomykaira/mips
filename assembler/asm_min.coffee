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
    when 'AND'  then '000000'
    when 'NOP'  then '000000'
    when 'OR'   then '000001'
    when 'ADD'  then '000010'
    when 'XOR'  then '000101'
    when 'SUB'  then '000110'
    when 'SLT'  then '000111'

    when 'ANDI' then '001000'
    when 'ORI'  then '001001'
    when 'ADDI' then '001010'
    when 'SLL'  then '001011'
    when 'SUBI' then '001110'
    when 'SLTI' then '001111'

    when 'FADD' then '010000'
    when 'FSUB' then '010001'
    when 'FMUL' then '010010'

    when 'F2I' then '010011'
    when 'I2F' then '010100'

    when 'SNDB'  then '000100'
    when 'RBYT'  then '001100'

    when 'LOAD+i' , 'LOAD-i', 'FLOAD+i', 'FLOAD-i' then '100011'
    when 'LOAD+r' , 'FLOAD+r'                      then '100100'
    when 'STORE+i', 'FSTORE+i'                     then '101011'
    when 'STORE+r', 'FSTORE+r'                     then '101100'

    when 'CALL'   then '110111'
    when 'RETURN' then '111000'

    when 'FBGE' then '111001'
    when 'FBLT' then '111010'
    when 'FBEQ'  then '111011'
    when 'BGE'  then '111100'
    when 'BLT'  then '111101'
    when 'BEQ'  then '111110'
    when 'JMP'  then '111111'
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

isRegister = (x) ->
  try
    reg(x)
    true
  catch e
    false

reg = (x) ->
  if match = x.match(/\%r(\d+)/)
    match[1].toBin(5)
  else if match = x.match(/\%f(\d+)/)
    (parseInt(match[1]) + 16).toBin(5)
  else
    throw "Register #{x} does not match \%r(\d+) or \%f(\d+)"

isNumber = (value) ->
  return false  if value instanceof Array

  #trim
  value = String(value).trim()
  return false if value.length is 0
  return false if isNaN(value)
  true

toInstruction = (line, labels) ->
  imm = (x, negate = false) ->
    x = labels[x] unless isNumber(x)
    (if negate then -x else x).toBin(16)


  match = line.match(/; pc = (\d+) ; ([^ ]*) ([^;]*);/)

  throw "#{line} is malformed" unless match

  pc   = parseInt(match[1])
  inst = match[2]
  args = match[3].split(" ").map (x) -> x.trim()

  label_abs = (labels, label) ->
    labels[label]

  label_relative = (labels, label, current) ->
    labels[label] - current - 1

  try
    switch inst
      when 'SLL'
        instCode(inst) + reg(args[1]) + reg(args[0]) + imm(args[2])
      when 'AND', 'OR', 'ADD', 'SUB', 'SLT', 'XOR'
        if isRegister(args[2])
          instCode(inst) + reg(args[1]) + reg(args[2]) + reg(args[0]) + '00000000000'
        else
          instCode(inst + 'I') + reg(args[1]) + reg(args[0]) + imm(args[2])
      when 'FADD', 'FSUB', 'FMUL'
        instCode(inst) + reg(args[1]) + reg(args[2]) + reg(args[0]) + '00000000000'
      when 'LOAD+', 'LOAD-', 'STORE+', 'STORE-', 'FLOAD+', 'FLOAD-', 'FSTORE+', 'FSTORE-'
        if isRegister(args[2])
          instCode(inst + "r") + reg(args[1]) + reg(args[0]) + reg(args[2]) + '00000000000'
        else
          instCode(inst + "i") + reg(args[1]) + reg(args[0]) + imm(args[2], inst[inst.length-1] == '-')
      when 'F2I', 'I2F'
        instCode(inst) + reg(args[1]) + reg(args[0]) + imm('0')
      when 'RBYT', 'SNDB'
        instCode(inst) + "00000" + reg(args[0]) + imm('0')
      when 'JMP', 'CALL'
        instCode(inst) + label_abs(labels, args[0]).toBin(26)
      when 'BGE', 'BLT', 'BEQ'
        instCode(inst) + reg(args[0]) + reg(args[1]) + label_relative(labels, args[2], pc).toBin(16)
      when 'FBGE', 'FBLT', 'FBEQ'
        instCode(inst) + reg(args[0]) + reg(args[1]) + label_relative(labels, args[2], pc).toBin(16)
      when 'NOP', 'RETURN'
        instCode(inst) + '0'.toBin(26)
      else
        throw "Unknown instruction #{inst}"
  catch e
    throw "#{e} in #{line}"

toMap = (constants) ->
  constants.split("\n").reduce (s, x) ->
    match = x.match(/"([^"]*)" = (\d+)/)
    if match
      s[match[1]] = parseInt(match[2])
    s
  , {}



contents = fs.readFileSync("/dev/stdin", encoding) # TODO: is this cross-platform?
[instructions, constants] = splitParts(contents)
constant_map = toMap(constants)
lines = instructions.split("\n").map (i) -> toInstruction i, constant_map
for i in [0..lines.length]
  if lines[i]
    console.log lines[i]
  else
    console.log ''
util.error("Instruction length: #{lines.length}")
