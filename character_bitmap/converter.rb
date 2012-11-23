#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# bitmap.txt を読んで verilog が読める形式のデータを生成する

file = File.readlines("bitmap.txt").select { |l| ! l.strip.empty? }

def write(bytes, comment)
  puts %q{%s} % [bytes.rjust(32, "0")]
end

bitmap_hash = {}

until file.empty?
  set = file.slice!(0, 17)
  letter = set[0].strip
  byte = set[1..16].map { |line| line[0..7].gsub(' ', '0').to_i(2).to_s(16) }.join('')

  bitmap_hash[letter] = byte
end

0.upto(127) do |i|
  if data = bitmap_hash[i.chr]
    write(data || '', i.chr)
  else
    write('', "(#{i})")
  end
end
