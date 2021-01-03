#!/usr/bin/ruby

require 'yaml'

data = YAML.safe_load File.read(ARGV[0])

def fix(data)
  '[ ' + data[0] + (', 0x%02x' % data[1]) + (', 0x%02x]' % data[2])
end

'ABCDEFGHV'.each_char do |character|
  char_data = data.dig(character, ARGV[2].to_i)
  next if char_data.nil? || char_data[0] != ARGV[1]
  puts character + ': ' + fix(char_data)
end
