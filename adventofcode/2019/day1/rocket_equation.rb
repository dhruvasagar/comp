def fuel_required(mass)
  (mass / 3) - 2
end

def total_fuel_required(mass)
  fr = fuel_required(mass)
  return 0 if fr < 0

  fr + total_fuel_required(fr)
end

sum = 0
while (mass = gets.to_i) != 0
  sum += total_fuel_required(mass)
end

puts sum
