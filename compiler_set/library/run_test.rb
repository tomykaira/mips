#!/usr/bin/env ruby

# run_test.rb TARGET START END STEP

sim_tester = File.expand_path(File.join(__FILE__, '..', 'create_test_bench.rb'))

if ARGV.length != 4
  puts "run_test.rb TARGET START END STEP"
  exit 1
end

target = ARGV[0]
start, stop, step = ARGV[1..3].map(&:to_f)
test_values = []
while (start < stop)
  test_values << start
  start += step
end

result_ruby = []
result_sim = []

if Math.respond_to?(target)
  result_ruby = test_values.map { |v| Math.send(target, v) }
else
  STDERR.puts "Method #{target} is unknown"
  exit 1
end

IO.popen("ruby #{sim_tester} #{target}", 'r+') do |pipe|
  test_values.each{ |v| pipe.write "#{v}\n" }
  pipe.close_write
  result_sim = pipe.readlines.map(&:to_f)
end

test_values.each_with_index do |v, i|
  if (result_ruby[i] - result_sim[i]).abs > (result_ruby[i] * 0.001)
    puts "#{v}\truby: #{result_ruby[i]}\tsim: #{result_sim[i]}"
  end
end
