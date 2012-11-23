#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require 'timeout'

# 最長で実行を待つ時間。これを越えたら失敗する
# min-rt の実行がかなり時間かかる
TIMEOUT = 300

if ARGV.size == 0
  STDERR.puts "Specify test group you want to run"
  STDERR.puts "\ttest: run all tests on simulator"
  STDERR.puts "\tcore: run all tests on latest mimic core"
end

case ARGV[0]
when 'test'
  target_directories = ['small', 'compare', 'io', 'large', 'min-rt', 'Loreley/test/with-ocaml', 'Loreley/test/with-answer']
  task_suffix = 'test'
when 'core'
  target_directories = ['asm', 'small', 'compare', 'io', 'large', 'min-rt']
  task_suffix = 'test_core'
when 'min-rt'
  target_directories = ['min-rt']
  task_suffix = 'test'
else
  STDERR.puts "Unknown test mode.  \"test\" or \"core\""
end

NG = []

def make_with_timeout(task, timeout)
  r, w = IO.pipe
  pid = spawn("make #{task}", :out => w, :err => [:child, :out])
  w.close

  success =
    begin
      Timeout::timeout(TIMEOUT) do
      Process.waitpid(pid)
      $?.success?
    end
    rescue Timeout::Error
      Process.kill(:SIGTERM, pid)
      false
    end

  if success
    puts "#{task}: OK"
  else
    puts "#{task}: NG"
    output = begin
               r.read_nonblock(65535)
             rescue IO::WaitReadable
               "-- Output not available"
             end
    NG << [task, output]
  end
end

target_directories.each do |dir|
  puts dir
  Dir.chdir "test/#{dir}" do

    `make targets`.split("\n").each do |target|
      make_with_timeout("#{target.gsub(/\.[a-z]*$/, '')}.#{task_suffix}", 30)
    end
  end
end

NG.each do |file, reason|
  puts file
  puts reason
end

if NG.empty?
  exit 0
else
  exit 1
end
