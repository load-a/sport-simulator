#!/usr/bin/env ruby
# frozen_string_literal: true

MAIN_PATH = 'src/main/cobol'
TEST_PATH = 'src/test/cobol'

def build_main 
  puts "COMPILING MAIN FILES..."
  Dir.each_child(MAIN_PATH) do |file|
    `cobc -x -o build/#{File.basename(file, ".cob")} #{MAIN_PATH}/#{file}`
  end
end

def build_tests
  puts 'COMPLILING TESTS...'
  Dir.each_child(TEST_PATH) do |test_file|
    `cobc -x -o src/test/build/#{File.basename(test_file, ".cob")} #{TEST_PATH}/#{test_file}`
  end
end

case ARGV[0]&.downcase
when '-t'
  build_tests
when '-m'
  build_main
when '+h', '-h'
  puts "-t: Build tests\n-m: build main files\n+h: this help text\n<none> build all"
else
  build_main
  build_tests
end
