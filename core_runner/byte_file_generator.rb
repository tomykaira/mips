#!/usr/bin/env ruby

target = ARGV[0]
File.open(target.gsub(/\.bin$/, '.byte.bin'), 'w') do |f|
  File.readlines(target).each do |line|
    inst, = line.split("\t")
    code = inst.to_i(16)
    f.write [code].pack('N')
  end
  f.write [0xffffffff].pack('N')
end
