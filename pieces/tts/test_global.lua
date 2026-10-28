--[[ Run the mod's Global script outside Tabletop Simulator, against a small
     stand-in for the parts of TTS's API it uses, and set up every player
     count. Catches what a parser cannot: nil lookups, wrong keys into the
     config, pieces landing on the board. It cannot say how anything looks.

       lua5.2 pieces/tts/test_global.lua pieces/tts/out/Organism.json

     JSON is turned into Lua by python3, as TTS's own JSON is not here. ]]

local save_path = arg[1] or "pieces/tts/out/Organism.json"

local function json_to_lua(text)
  local tmp = os.tmpname()
  local f = assert(io.open(tmp, "w")); f:write(text); f:close()
  local p = assert(io.popen("python3 -c '" .. [[
import json, sys
def lua(v):
    if isinstance(v, dict):
        return "{" + ",".join("[%s]=%s" % (json.dumps(k, ensure_ascii=False), lua(x)) for k, x in v.items()) + "}"
    if isinstance(v, list):
        return "{" + ",".join(lua(x) for x in v) + "}"
    if v is None: return "nil"
    if v is True: return "true"
    if v is False: return "false"
    return json.dumps(v, ensure_ascii=False)
print("return " + lua(json.load(open(sys.argv[1]))))
]] .. "' " .. tmp))
  local src = p:read("*a"); p:close(); os.remove(tmp)
  return assert(load(src))()
end

JSON = {decode = json_to_lua, encode = function() return "{}" end}

-- ── Vectors ──────────────────────────────────────────────────────────────
local V = {}
V.__index = V
function Vector(x, y, z)
  if type(x) == "table" then x, y, z = x[1] or x.x, x[2] or x.y, x[3] or x.z end
  return setmetatable({x = x, y = y, z = z}, V)
end
V.__add = function(a, b) return Vector(a.x + b.x, a.y + b.y, a.z + b.z) end
V.__sub = function(a, b) return Vector(a.x - b.x, a.y - b.y, a.z - b.z) end
V.__mul = function(a, k) return Vector(a.x * k, a.y * k, a.z * k) end
function V:normalized()
  local m = math.sqrt(self.x ^ 2 + self.y ^ 2 + self.z ^ 2)
  return Vector(self.x / m, self.y / m, self.z / m)
end

-- ── Objects ──────────────────────────────────────────────────────────────
-- A custom tile is TILE units across at scale 1 (a guess; the script must
-- not depend on it), a model 1.
local TILE = 2.0
local objects, snaps, guid = {}, {}, 0

local function make(data, position, rotation)
  guid = guid + 1
  local o = {data = data, pos = Vector(position), rot = Vector(rotation or {0, 0, 0}),
             scale = Vector(data.Transform.scaleX, data.Transform.scaleY, data.Transform.scaleZ),
             tags = data.Tags or {}, guid = string.format("%06d", guid), destroyed = false,
             loading_custom = false, spawning = false, resting = true}
  local base = data.Name == "Custom_Tile" and TILE or 1
  function o.getBoundsNormalized()
    return {size = Vector(o.scale.x * base, o.scale.y * 0.2, o.scale.z * base)}
  end
  function o.getScale() return Vector(o.scale.x, o.scale.y, o.scale.z) end
  function o.setScale(s) o.scale = Vector(s) end
  function o.getPosition() return Vector(o.pos.x, o.pos.y, o.pos.z) end
  function o.positionToWorld(l)
    l = Vector(l)
    local x, y, z = l.x * o.scale.x, l.y * o.scale.y, l.z * o.scale.z
    if o.rot.z == 180 then x, y = -x, -y end
    return Vector(o.pos.x + x, o.pos.y + y, o.pos.z + z)
  end
  function o.getGMNotes() return data.GMNotes end
  function o.getName() return data.Nickname end
  function o.getValue() return data.FogColor end
  function o.setPosition(p) o.pos = Vector(p) end
  function o.setRotation(r) o.rot = Vector(r) end
  function o.hasTag(tag)
    for _, t in ipairs(o.tags) do if t == tag then return true end end
    return false
  end
  function o.setPositionSmooth(p) o.pos = Vector(p) end
  function o.setRotationSmooth(r) o.rot = Vector(r) end
  function o.getGUID() return o.guid end
  function o.setLock() end
  function o.isDestroyed() return o.destroyed end
  function o.destruct() o.destroyed = true end
  table.insert(objects, o)
  return o
end

function spawnObjectData(p)
  local o = make(p.data, p.position, p.rotation)
  if p.callback_function then p.callback_function(o) end
  return o
end
function getObjects()
  local live = {}
  for _, o in ipairs(objects) do if not o.destroyed then table.insert(live, o) end end
  return live
end
function getObjectsWithTag(tag)
  local found = {}
  for _, o in ipairs(getObjects()) do
    for _, t in ipairs(o.tags) do if t == tag then table.insert(found, o) end end
  end
  return found
end
function getObjectFromGUID() return nil end
Wait = {condition = function(fn, cond) assert(cond(), "waited on a condition that never holds"); fn() end,
        frames = function(fn) fn() end}
Tables = {getTableObject = function()
  return {getBounds = function() return {center = Vector(0, 0, 0), size = Vector(52, 5, 52)} end}
end}
Hands = {getHands = function()
  local hands = {}
  for _, o in ipairs(getObjects()) do if o.data.Name == "HandTrigger" then table.insert(hands, o) end end
  return hands
end}
Global = {setSnapPoints = function(points) snaps = points end}
UI = {getAttribute = function() end, setAttribute = function() end}
function broadcastToAll(msg) print("  > " .. msg) end

-- ── Run ──────────────────────────────────────────────────────────────────
local f = assert(io.open(save_path)); local save = JSON.decode(f:read("*a")); f:close()
assert(load(save.LuaScript, "global.lua"))()
for _, data in ipairs(save.ObjectStates) do
  local t = data.Transform
  make(data, {t.posX, t.posY, t.posZ}, {t.rotX, t.rotY, t.rotZ})
end
onLoad("")

local failures = 0
local function check(ok, msg)
  if not ok then failures = failures + 1; print("  FAIL " .. msg) end
end

for n = 2, 6 do
  print(n .. " players")
  setup(n)
  local key = config.plans[tostring(n)]
  local b = config.boards[key]
  local count = 0
  for _ in pairs(b.spaces) do count = count + 1 end
  check(#snaps == count, ("snap points %d, spaces %d"):format(#snaps, count))
  local board, by = nil, {}
  for _, o in ipairs(getObjectsWithTag(TAG)) do
    local name = o.data.Nickname
    by[o.data.Name] = (by[o.data.Name] or 0) + 1
    if name:match("^Board") then board = o end
  end
  check(board ~= nil, "no board")
  local across = board.getBoundsNormalized().size.x
  check(math.abs(across - config.templates.board_inches[b.object]) < 0.02,
        ("board %.2f in across, wants %.2f"):format(across, config.templates.board_inches[b.object]))
  -- every snap point on the board's face, every element just off it
  local r = across / 2
  for _, s in ipairs(snaps) do
    check(math.sqrt(s.position.x ^ 2 + s.position.z ^ 2) < r, "snap point off the board")
  end
  for _, o in ipairs(getObjectsWithTag(TAG)) do
    local d = math.sqrt(o.pos.x ^ 2 + o.pos.z ^ 2)
    if o.data.Name == "Custom_Model" and o.data.Nickname:match("[EM][AO][TV]E?$") then
      check(d > r * 0.85 and d < r + 3, ("%s at %.1f, board radius %.1f"):format(o.data.Nickname, d, r))
    end
  end
  -- the shared things clear of every player's things
  for _, s in ipairs(getObjects()) do
    if ({["Power board"] = 1, Mutations = 1, Food = 1})[s.getName()] and not s.hasTag(TAG) then
      for _, o in ipairs(getObjectsWithTag(TAG)) do
        local d = math.sqrt((o.pos.x - s.pos.x) ^ 2 + (o.pos.z - s.pos.z) ^ 2)
        if not o.data.Nickname:match("power level") and not o.data.Nickname:match("^Board") then
          check(d > 3, ("%s %.1f from %s"):format(o.data.Nickname, d, s.getName()))
        end
      end
    end
  end
  -- each player's seat behind their own pieces, no two seats on top of each other
  local seats = {}
  for i = 1, n do
    local want = config.seats[config.players[i]]
    for _, h in ipairs(Hands.getHands()) do if h.getValue() == want then seats[i] = h end end
    check(seats[i] ~= nil, "no seat " .. want)
  end
  for _, a in ipairs(Hands.getHands()) do
    for _, c in ipairs(Hands.getHands()) do
      if a ~= c then
        check(math.sqrt((a.pos.x - c.pos.x) ^ 2 + (a.pos.z - c.pos.z) ^ 2) > 10,
              ("seats %s and %s overlap"):format(a.getValue(), c.getValue()))
      end
    end
  end
  local summary = {}
  for k, v in pairs(by) do table.insert(summary, k .. " " .. v) end
  table.sort(summary)
  print(("  board %s (%s face), %.1f in; %d snap points; %s"):format(
    key, b.face, across, #snaps, table.concat(summary, ", ")))
end
print(failures == 0 and "ok" or (failures .. " failures"))
os.exit(failures == 0 and 0 or 1)
