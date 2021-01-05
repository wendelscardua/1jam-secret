#!/usr/bin/ruby

require 'yaml'
require 'pry'

data = YAML.safe_load File.read(ARGV[0])
memory_data = YAML.safe_load File.read(ARGV[1])

characters = 'ABCDEFGHV'.each_char.with_index.to_h
rooms = 'zabcdefghi'.each_char.with_index.to_h
fpm = 60

def timestamp_to_index(timestamp)
  start = 18 * 60 + 0
  (timestamp / 100) * 60 + timestamp % 100 - start
end

def index_to_timestamp(index)
  start = 18 * 60 + 0
  minutes = start + index
  (minutes / 60) * 100 + minutes % 60
end

last_time_index = timestamp_to_index(2200)

timeline = Array.new(last_time_index + 1) do
  {}
end

characters.each do |character, _|
  char_data = data[character].transform_keys { |key| timestamp_to_index(key) }
  indices = char_data.keys.sort
  (0..last_time_index).each do |time_index|
    timeline[time_index][character] =
      if indices.include?(time_index)
        room, x, y = char_data[time_index]
        [rooms[room], x, y]
      else
        prev_idx = indices.select { |idx| idx < time_index }.max
        next_idx = indices.select { |idx| idx > time_index }.min
        room1, x1, y1 = char_data[prev_idx]
        room2, x2, y2 = char_data[next_idx]
        raise "Invalid data for #{character}, missing between #{index_to_timestamp(prev_idx)} and #{index_to_timestamp(next_idx)}" if room1 != room2

        [
          rooms[room1],
          (((x2 - x1).to_f / (next_idx - prev_idx)) * (time_index - prev_idx) + x1).round.to_i,
          (((y2 - y1).to_f / (next_idx - prev_idx)) * (time_index - prev_idx) + y1).round.to_i
        ]
      end
  end
end

keyframe_addrs = (0..last_time_index).map { |idx| ts = index_to_timestamp(idx); "keyframe_#{ts}" }
                                     .join(', ')

puts ".define keyframe_lt #{keyframe_addrs}"
puts 'keyframe_lt_l: .lobytes keyframe_lt'
puts 'keyframe_lt_h: .hibytes keyframe_lt'
(0..last_time_index).each do |index|
  timestamp = index_to_timestamp(index)
  bytes = timeline[index].flat_map do |character, char_data|
    room, x, y = char_data
    delta_x, delta_y = if index == last_time_index ||
                          room != timeline[index + 1][character][0]
                         [0, 0]
                       else
                         (1..2).map do |i|
                           delta = (timeline[index][character][i] - timeline[index + 1][character][i]).abs
                           delta = delta.to_f / fpm
                           raise "Huge delta_#{i}: #{delta} at timestamp #{index_to_timestamp(index)}, char #{character}"  if delta.to_i > 0

                           (delta * 256).to_i
                         end
                       end
    [room, x, y, delta_x, delta_y]
  end.map { |byte| '$%02x' % byte }.join(', ')
  puts "keyframe_#{timestamp}: .byte #{bytes}"
end

clock_digits = (0..last_time_index).map { |i| index_to_timestamp(i).to_s.chars }
                                   .transpose
                                   .map
                                   .with_index { |bytes, index| %Q{clock_digits_#{index}: .byte "#{bytes.join}"} }
puts clock_digits

alethioscope_data_pointers = memory_data.keys.map { |key| "alethioscope_data_#{key}" }.join(', ')
puts ".define alethioscope_data_pointers #{alethioscope_data_pointers}"
puts 'alethioscope_data_pointers_l: .lobytes alethioscope_data_pointers'
puts 'alethioscope_data_pointers_h: .hibytes alethioscope_data_pointers'

# check memory_data consistency
checklist = Array.new(last_time_index + 1) { Array.new(8) { '.' } }

memory_data.each_with_index do |(_character, ranges), char_index|
  ranges.flat_map { |range| (range[0]..range[1]).to_a }
        .map { |frame| timestamp_to_index(frame) }
        .each do |frame|
    # mark an "x" for every character/frame in the same room as this character/frame
    char_room = timeline[frame][char_index * 5]
    (0..7).each do |other_char_index|
      checklist[frame][other_char_index] = 'x' if timeline[frame][other_char_index * 5] == char_room
    end
  end
end

(0..last_time_index).each do |frame|
  $stderr.puts "#{index_to_timestamp(frame)} : #{checklist[frame].join}"
end

bad_ranges = false

checklist.transpose.each_with_index do |char_checklist, char_index|
  # compute and display uncovered ranges
  ranges = []
  (0..last_time_index).each do |frame|
    if char_checklist[frame] != 'x'
      ranges << (frame..frame)
    end
  end
  raise "C must have a range" if ranges.empty? && char_index == 2
  next if ranges.empty?

  i = 0
  while ranges.size > 1 && i < ranges.size - 1
    if ranges[i].end + 1 == ranges[i + 1].begin
      a = ranges[i]
      b = ranges.delete_at(i + 1)
      ranges[i] = a.begin..b.end
      next
    end
    i += 1
  end
  char = characters.keys.sort[char_index]
  $stderr.puts "Uncovered ranges for #{char}: #{ranges.map { |r| (index_to_timestamp(r.begin)..index_to_timestamp(r.end)) }.inspect}"
  bad_ranges = true if char_index != 2 ||
                       ranges.any? { |range| range != (timestamp_to_index(2040)..timestamp_to_index(2059)) }
end

raise "Bad range(s)" if bad_ranges

memory_data.each do |key, frames|
  frames_bytes = [
    frames[0][0], frames[1][0], frames[2][0],
    frames[0][1], frames[1][1], frames[2][1]
  ].map { |timestamp| timestamp_to_index(timestamp) }
   .map { |byte| '$%02x' % byte }
   .join(', ')
  puts "alethioscope_data_#{key}: .byte #{frames_bytes}"
end
