#!/usr/bin/ruby

require 'yaml'
require 'pry'

def timestamp_to_index(timestamp)
  start = 18 * 60 + 0
  (timestamp / 100) * 60 + timestamp % 100 - start
end

def index_to_timestamp(index)
  start = 18 * 60 + 0
  minutes = start + index
  (minutes / 60) * 100 + minutes % 60
end

data = YAML.safe_load File.read(ARGV[0])

characters = 'ABCDEFGHV'.each_char.with_index.to_h
rooms = 'zabcdefghi'.each_char.with_index.to_h

timeline = Array.new(241) do
  {}
end

characters.each do |character, _|
  char_data = data[character].transform_keys { |key| timestamp_to_index(key) }
  indices = char_data.keys.sort
  (0..240).each do |time_index|
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

def random_range
  size = 3
  size += 1 if rand < 0.25
  # size += 1 if rand < 0.25

  b = rand(25 - size)
  r_start = 10 * b
  r_end = 10 * b + size * 10 - 1
  r_end = 240 if r_end == 239
  r_start..r_end
end

def random_gene
  24.times.map { random_range }
end

JACKPOT = 1_000_000

def gene_to_data(gene)
  gene.each_slice(3)
      .with_index
      .map do |k, v|
    ['ABCDEFGH'[v], k]
  end
end

def score(timeline, gene)
  checklist = Array.new(241) { Array.new(8) { '.' } }

  memory_data = gene_to_data(gene)

  score = 0

  memory_data.each do |character, ranges|
    ranges.flat_map { |range| range.to_a }
          .each do |frame|
      
      char_room = timeline[frame][character][0]
      ('A'..'H').each_with_index do |other_character, other_char_index|
        if timeline[frame][other_character][0] == char_room ||
           timeline[frame][other_character][0] == 0 # outside doesn't matter
          if checklist[frame][other_char_index] != 'x'
            checklist[frame][other_char_index] = 'x'
            score += 1
          end 
        end
      end
    end
  end

  (timestamp_to_index(2040)..timestamp_to_index(2059)).each do |frame|
    if checklist[frame][2] == 'x'
      score /= 2
    end
  end

  if score >= 8 * 241 - 20
    JACKPOT
  else
    score
  end
end

def random_sample(population)
  total_weight = population.sum { |_, score| score }
  random_weight = rand(total_weight)
  i = 0
  while i < population.length
    random_weight -= population[i][1]
    if random_weight.negative?
      return population[i]
    end
    i += 1
  end
  population.sample
end

def mutate_population(timeline, population)
  new_pop = [population.first]
  (population.size - 1).times do
    mom = random_sample(population)[0]
    dad = random_sample(population)[0]
    cross = rand(24)
    child = mom[0...cross] + dad[cross...24]
    if rand < 0.06
      mut = random_range
      index = (0..23).map do |i|
        duped = child.dup
        duped[i] = mut
        [i, score(timeline, duped)]
      end.max_by { |_i, score| score }[0]
      child[index] = mut
    elsif rand < 0.04
      child[rand(24)] = random_range
    elsif rand < 0.05
      i = rand(24)
      j = rand(24)
      child[i], child[j] = child[j], child[i]
    end
    new_pop << [child, 0]
  end
  new_pop
end

population_size = 100

population = Array.new(population_size) { [random_gene, 0.0] }

best = 0
generation = 0

loop do
  population = population.map { |gene, _score| [gene, score(timeline, gene)] }
                         .sort_by { |_, score| score }
                         .reverse

  current_score = population.first[1]
  generation += 1
  if generation % 20 == 0 || current_score == JACKPOT || current_score > best
    puts "Generation #{generation} - delta #{current_score - best}"
    puts population.first.inspect
    puts population.map { |pop| pop[1] }.inspect
    if current_score > best
      File.open('./best-so-far.yaml', 'wb') do |f|
        f.write gene_to_data(population.first[0].map { |i| [index_to_timestamp(i.begin), index_to_timestamp(i.end)] }).to_h.to_yaml
      end
    end
    best = current_score
    break if current_score == JACKPOT
  end

  max_score = population.map { |_gene, score| score }.max
  min_score = population.map { |_gene, score| score }.min

  if max_score > min_score
    population = population.map do |gene, score|
      [gene, ((score - min_score).to_f / (max_score - min_score)) ** 2]
    end
  else
    population = population.map do |gene, _score|
      [gene, 1.0]
    end
  end
  
  population = mutate_population(timeline, population)
end

binding.pry
