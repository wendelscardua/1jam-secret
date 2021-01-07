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

def asm_array(array)
  array.map { |byte| format('$%02x', byte) }
       .join(', ')
       .then { |byte_string| ".byte #{byte_string}" }
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
        if room1 != room2
          raise "Invalid data for #{character}, missing between #{index_to_timestamp(prev_idx)} and #{index_to_timestamp(next_idx)}"
        end

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
                           if delta.to_i > 0
                             raise "Huge delta_#{i}: #{delta} at timestamp #{index_to_timestamp(index)}, char #{character}"
                           end

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
                                   .with_index { |bytes, index| %(clock_digits_#{index}: .byte "#{bytes.join}") }
puts clock_digits

# check memory_data consistency
checklist = Array.new(last_time_index + 1) { Array.new(8) { '.' } }

memory_data.each do |character, ranges|
  ranges.flat_map { |range| (timestamp_to_index(range[0])..timestamp_to_index(range[1])).to_a }
        .each do |frame|
    # mark an "x" for every character/frame in the same room as this character/frame
    char_room = timeline[frame][character][0]
    ('A'..'H').each_with_index do |other_character, other_char_index|
      if timeline[frame][other_character][0] == char_room ||
         timeline[frame][other_character][0] == 0 # outside doesn't matter
        checklist[frame][other_char_index] = 'x'
      end
    end
  end
end

(0..last_time_index).each do |frame|
  warn "#{index_to_timestamp(frame)} : #{checklist[frame].join}"
end

bad_ranges = false

checklist.transpose.each_with_index do |char_checklist, char_index|
  # compute and display uncovered ranges
  ranges = []
  (0..last_time_index).each do |frame|
    ranges << (frame..frame) if char_checklist[frame] != 'x'
  end
  raise 'C must have a range' if ranges.empty? && char_index == 2
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
  warn "Uncovered ranges for #{char}: #{ranges.map do |r|
                                          (index_to_timestamp(r.begin)..index_to_timestamp(r.end))
                                        end.inspect}"
  bad_ranges = true if char_index != 2 ||
                       ranges.any? { |range| range != (timestamp_to_index(2040)..timestamp_to_index(2059)) }
end

raise 'Bad range(s)' if bad_ranges

# padding frames with zeroes so index = character << 2 + (0..2)
start_frames = memory_data.flat_map { |_key, frames| [frames[0][0], frames[1][0], frames[2][0], 1800] }
                          .map { |frame| timestamp_to_index(frame) }
end_frames = memory_data.flat_map { |_key, frames| [frames[0][1], frames[1][1], frames[2][1], 1800] }
                        .map { |frame| timestamp_to_index(frame) }
rooms = memory_data.flat_map do |key, frames|
  [frames[0][0], frames[1][0], frames[2][0]].map { |frame| timeline[timestamp_to_index(frame)][key][0] } +
    [0]
end

puts "alethioscope_start_frames: #{asm_array(start_frames)}"
puts "alethioscope_end_frames: #{asm_array(end_frames)}"
puts "alethioscope_rooms: #{asm_array(rooms)}"
