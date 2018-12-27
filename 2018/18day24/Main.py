# Switching to python... sorry. Haskell was too much to handle.
import re
import copy

# 11573 for A too low
# 17734 for A too low
# 17738 for A was right
# 17739 for A too high

def parse(text):
  battle = []
  faction = None
  counter = dict()
  for line in text.splitlines():
    if line == "": continue
    if line == "Immune System:":
      faction = "immune"
      counter[faction] = 1
      continue
    if line == "Infection:":
      faction = "infection"
      counter[faction] = 1
      continue
    match = re.match(r'^\s*(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)$', line)
    if match == None:
      raise Exception("failed to parse: '%s'"%line)
      continue
    group = {
      "faction": faction,
      "idx": counter[faction],
      "count": int(match.group(1)),
      "hp": int(match.group(2)),
      "damage": int(match.group(4)),
      "damage_type": match.group(5),
      "initiative": int(match.group(6)),
      "immune": [],
      "weak": [],
    }
    buffstr = match.group(3)
    if buffstr and len(buffstr) > 0:
      buffstr = buffstr[1:-2]
      for buffs in buffstr.split(";"):
        key, values = buffs.strip().split(" to ")
        group[key] = values.split(", ")
    battle.append(group)
    counter[faction] += 1
  return battle

def effective_power(group):
  return group["count"] * group["damage"]

def potential_damage(attacker, defender):
  if attacker["damage_type"] in defender["immune"]: return 0
  dmg = attacker["damage"]
  if attacker["damage_type"] in defender["weak"]: return dmg * 2
  return dmg

def fight(battle):
  # compute new ep for group
  for idx, group in enumerate(battle):
    group["effective_power"] = effective_power(group)
    group["_idx"] = idx
    group["target"] = None
  # select targets:
  #  sort by effective power, initiative
  defenders = battle.copy()
  attackers = sorted(battle, key=lambda group: (group["effective_power"], group["initiative"]), reverse=True)
  #  select target from list based on max damage, largest ep, highest init
  for attacker in attackers:
    target_idx = None
    max_damage = 0
    for defender in defenders:
      if defender["faction"] == attacker["faction"]: continue
      damage = potential_damage(attacker, defender) * attacker["count"]
      if damage == 0 or damage < max_damage: continue
      if target_idx == None or damage > max_damage:
        target_idx = defender["_idx"]
        max_damage = damage
        continue
      other = battle[target_idx]
      if defender["effective_power"] < other["effective_power"]: continue
      if defender["effective_power"] > other["effective_power"]:
        target_idx = defender["_idx"]
        continue
      if defender["initiative"] > other["initiative"]:
        target_idx = defender["_idx"]
        continue
    if target_idx == None: continue
    defenders = [defender for defender in defenders if defender["_idx"] != target_idx]
    attacker["target"] = target_idx
  # attack:
  #   deal damage based on initiative
  stale = True
  attackers = sorted(attackers, key=lambda group: group["initiative"], reverse=True)
  for attacker in attackers:
    if attacker["target"] == None: continue
    actual_attacker = battle[attacker["_idx"]]
    if actual_attacker["count"] <= 0: continue
    actual_defender = battle[attacker["target"]]
    damage = potential_damage(actual_attacker, actual_defender)
    damage *= actual_attacker["count"]
    hp = actual_defender["hp"]
    dead_units = min(actual_defender["count"], int(int(damage) / int(hp)))
    if dead_units > 0: stale = False
    actual_defender["count"] -= dead_units
  if stale: return [] # just hack the battle empty for now when stale.
  # return alive groups
  return [group for group in battle if group["count"] > 0]

def is_over(battle):
  def count(faction):
    return len([group for group in battle if group["faction"] == faction])
  return count("immune") == 0 or count("infection") == 0

def pretty_print(battle):
  for faction in ["immune", "infection"]:
    print("%s:"%faction)
    for group in battle:
      if group["faction"] != faction: continue
      print("Group %s contains %s units"%(group["idx"], group["count"]))
  print()

def run_a(text):
  battle = parse(text)
  while not is_over(battle):
    battle = fight(battle)
  # pretty_print(battle)
  return sum([group["count"] for group in battle])

def add_boost(battle, amount):
  for group in battle:
    if group["faction"] != "immune": continue
    group["damage"] += amount
  return battle

def run_b(text):
  proto_battle = parse(text)
  boost = 1
  while True:
    battle = copy.deepcopy(proto_battle)
    battle = add_boost(battle, boost)
    while not is_over(battle):
      battle = fight(battle)
    count = sum([group["count"] for group in battle if group["faction"] == "immune"])
    if count > 0:
      return count
    boost += 1

testInput = """\
Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
"""
assert run_a(testInput) == 5216

input_a = open("input.txt", "rt").read()
print("Solution for A: %s"%run_a(input_a))

assert run_b(testInput) == 51
print("Solution for B: %s"%run_b(input_a))
