def f(x)
  [x.to_i(16)].pack('I').unpack('f')
end

def d (x)
  [x].pack('f').unpack('I').first.to_s(16)
end

def f(x)
  puts "\tfmvhi $f5, #{x >> 16}"
  puts "\tfmvlo $f5, #{x & 0xffff}"
  puts "\tfmov $f0, $f5"
  puts "\tcall min_caml_exp"
  puts "\tfmov $f16, $f0"
end

bb = 1.upto(100).map { x = (rand * 2 - 1) * 10; [x, *d(x)] } # exp
bb = 1.upto(100).map { x = (rand) * 1000; [x, *d(x)] } # log
File.open("xxx", "w") { |f| f << bb.map(&:first).join("\n") }
