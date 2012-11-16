#!/usr/bin/env ruby

def path(*args)
  File.expand_path(File.join(__FILE__, '..', '..', *args))
end

ASSEMBLER = path('assembler', 'assembler')
SIMULATOR = path('simulator', 'simulator')

def int_to_float(x)
  [x.to_i(16)].pack('I').unpack('f')
end

def float_to_int(x)
  [x].pack('f').unpack('I').first
end

def generate_asm(fun_name, x)
  ["\tfmvhi $f0, #{x >> 16}",
    "\tfmvlo $f0, #{x & 0xffff}",
    "\tcall min_caml_#{fun_name}",
    "\tdebug 1"].join("\n")
end

File.open('/tmp/test.s', 'w') do |fp|
  fp.puts "\taddi $r1, $r0, 1024"
  STDIN.readlines.each do |f|
    fp.puts generate_asm(ARGV[0], float_to_int(f.to_f))
  end
  fp.puts "\thalt"
  fp.puts File.read(path('lib_asm.s'))
end

`#{ASSEMBLER} /tmp/test.s /tmp/test.bin`
system("#{SIMULATOR} -t /tmp/test.bin")
