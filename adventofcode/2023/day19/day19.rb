# frozen_string_literal: true

def parse_rule(rule)
  rule.split(',').map {|rc|
    next rc unless rc =~ /:/

    cond, rl = rc.split(':')
    pr, cmp, v = [$1, $2, $3] if cond =~ /(.*)([\<\>])(.*)/
    [pr, cmp, v.to_i, rl]
  }
end

def parse_input(lines)
  rules, ps = lines.split("\n\n").map(&:split)
  rmap = rules.reduce({}) {|m, rule|
    m[$1] = parse_rule($2) if rule =~ /(.*){(.*)}/
    m
  }
  parts = ps.reduce([]) {|pr, part|
    pr + [part[1..-2].split(',').reduce({}) {|m, kv|
      k, v = kv.split('=')
      m[k] = v.to_i
      m
    }]
  }
  [rmap, parts]
end

def rating(part) = part.values.sum
def apply_rule(rmap, part, rule='in')
  return 0 if rule == 'R'
  return rating(part) if rule == 'A'

  total = 0
  rmap[rule].each {|r|
    if r.is_a?(Array)
      pr, cmp, v, nrule = r
      if (cmp == '<' && part[pr] < v) || (cmp == '>' && part[pr] > v)
        total += apply_rule(rmap, part, nrule) 
        break
      end
    else
      total += apply_rule(rmap, part, r)
    end
  }
  total
end
def apply_rules(parts, rmap) = parts.reduce(0) {|s, p| s + apply_rule(rmap, p)}

def rating_range(part) = part.values.reduce(1) {|s, r| s * r.size}
def apply_rules_range(rmap, rule='in', part={"x" => (1..4000), "m" => (1..4000), "a" => (1..4000), "s" => (1..4000)})
  return 0 if rule == 'R'
  return rating_range(part) if rule == 'A'

  total = 0
  rmap[rule].each {|r|
      if r.is_a?(Array)
        pr, cmp, v, nrule = r
        range = part[pr]
        new_part = part.dup
        min, max = range.minmax
        if min < v && v < max
          if cmp == '<'
            new_part[pr] = (min...v)
            part[pr] = (v..max)
          else
            new_part[pr] = ((v + 1)..max)
            part[pr] = (min..v)
          end
          total += apply_rules_range(rmap, nrule, new_part)
        end
      else
        total += apply_rules_range(rmap, r, part)
      end
  }
  total
end

lines = ARGF.read
rmap, parts = parse_input(lines)
p apply_rules(parts, rmap)
p apply_rules_range(rmap)
