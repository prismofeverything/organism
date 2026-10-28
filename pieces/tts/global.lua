--[[ ORGANISM for Tabletop Simulator.

     Built by pieces/tts/build_tts.py from pieces/tts/global.lua -- edit that
     and rebuild, not this copy in the game.

     CONFIG_JSON (filled in by the build) holds each board's spaces, in image
     coordinates (u right, v down, -1..1 across the image), the home spaces of
     every player count, and the objects Setup spawns. ]]

--CONFIG--

TAG = "OrganismSetup"
ELEMENTS = {"EAT", "GROW", "MOVE"}

-- How image coordinates lie on a tile's local axes, per face, as measured on
-- a real table: TTS lays a tile's front image turned half round, and flipping
-- the tile (about z) mirrors x for the back.
ORIENT = {front = {u = -1, v = 1}, back = {u = 1, v = 1}}

config = nil
state = {}

-- ── Helpers ───────────────────────────────────────────────────────────────

function whenLoaded(object, fn)
  Wait.condition(function() fn(object) end,
    function() return object.isDestroyed() or (not object.loading_custom and not object.spawning) end)
end

-- Objects that carry "inches=N" in their GM notes are scaled so they measure N
-- inches across, whatever size TTS makes a custom tile at scale 1.
function fitSize(object, done)
  local inches = tonumber(string.match(object.getGMNotes() or "", "inches=([%d%.]+)"))
  if not inches then
    if done then done(object) end
    return
  end
  whenLoaded(object, function(o)
    local size = o.getBoundsNormalized().size
    local across = math.max(size.x, size.z)
    if across > 0 and math.abs(across - inches) > 0.01 then
      local s = o.getScale()
      local k = inches / across
      o.setScale({s.x * k, s.y, s.z * k})
    end
    if done then Wait.frames(function() done(o) end, 2) end
  end)
end

function spawn(data, position, rotation, done)
  return spawnObjectData({
    data = data, position = position, rotation = rotation or {0, 0, 0},
    callback_function = function(o) fitSize(o, done) end})
end

-- A space's table position: the board tile's top face at image point uv.
function spaceWorld(board, face, uv)
  local s = board.getScale()
  local size = board.getBoundsNormalized().size
  local o = ORIENT[face]
  local up = (face == "back") and -1 or 1
  return board.positionToWorld({
    o.u * uv[1] * size.x / 2 / s.x,
    up * size.y / 2 / s.y,
    o.v * uv[2] * size.z / 2 / s.z})
end

function setSnaps(board, b)
  local points = {}
  for _, uv in pairs(b.spaces) do
    table.insert(points, {position = spaceWorld(board, b.face, uv), rotation_snap = false})
  end
  Global.setSnapPoints(points)
end

-- ── Setup ─────────────────────────────────────────────────────────────────

-- The table's top surface, so things spawn just above it whatever the table.
function tableTop()
  local b = Tables.getTableObject().getBounds()
  return b.center.y + b.size.y / 2
end

-- The shared things -- power board, mutations, food -- go in the
-- widest gap between the players, out past the board.
function placeShared(centre, radius, angles, n)
  table.sort(angles)
  local best, at = -1, 0
  for i, a in ipairs(angles) do
    local b = angles[i % #angles + 1] + ((i == #angles) and 2 * math.pi or 0)
    if b - a > best then best, at = b - a, (a + b) / 2 end
  end
  local d = Vector(math.sin(at), 0, math.cos(at))
  local side = Vector(-d.z, 0, d.x)
  local top = tableTop()
  local place = {
    ["Food"] = d * (radius + 2.6),
    ["Mutations"] = d * (radius + 5.2),
    ["Power board"] = d * (radius + 10)}
  for _, o in ipairs(getObjects()) do
    local p = place[o.getName()]
    if p and not o.hasTag(TAG) then
      o.setLock(false)
      o.setPositionSmooth({centre.x + p.x, top + 0.5, centre.z + p.z}, false, true)
      o.setRotationSmooth({0, math.deg(at) + 180, 0}, false, true)
      if o.getName() == "Power board" then
        Wait.condition(function() o.setLock(true) end, function() return o.resting end)
      end
    end
  end
  return centre + d * (radius + 10), side
end

-- Each player's seat (hand zone) at the table's edge behind their pieces,
-- facing the board; the seats of colours not playing wait at the far side.
function placeSeats(centre, radius, players, n)
  local top = tableTop()
  local used = {}
  for i, d in ipairs(players) do
    local seat = config.seats[config.players[i]]
    used[seat] = d
  end
  local spare = 0
  for _, h in ipairs(Hands.getHands()) do
    local d = used[h.getValue()]
    if d then
      local at = centre + d * (radius + 12)
      h.setPosition({at.x, top + 2.6, at.z})
      h.setRotation({0, math.deg(math.atan2(-d.x, -d.z)), 0})
    else
      spare = spare + 1
      h.setPosition({-46 + spare * 13, top + 2.6, 33})
      h.setRotation({0, 180, 0})
    end
  end
end

function clear()
  for _, o in ipairs(getObjectsWithTag(TAG)) do o.destruct() end
  Global.setSnapPoints({})
end

-- Each player's elements wait just outside their home spaces, one food on
-- each: INTRODUCE is theirs to arrange. Their supply bags and tokens sit
-- further out on the same line; power levels stand beside the power board.
function placePlayers(board, b, n)
  local t = config.templates
  local centre = board.getPosition()
  local radius = t.board_inches[b.object] / 2
  local top = tableTop()
  local players, angles = {}, {}
  for i, home in ipairs(b.homes[tostring(n)]) do
    local mid = spaceWorld(board, b.face, b.spaces[home[2]])
    local d = Vector(mid.x - centre.x, 0, mid.z - centre.z):normalized()
    players[i] = d
    angles[i] = math.atan2(d.x, d.z)
  end
  local power, along = placeShared(centre, radius, {table.unpack(angles)}, n)
  placeSeats(centre, radius, players, n)
  for i, home in ipairs(b.homes[tostring(n)]) do
    local color = config.players[i]
    local p = t.players[color]
    local d = players[i]
    local side = Vector(-d.z, 0, d.x)
    local yaw = math.deg(math.atan2(d.x, d.z)) + 180
    for j, space in ipairs(home) do
      -- in line with its home space, just past the board's edge
      local at = spaceWorld(board, b.face, b.spaces[space])
      local rel = Vector(at.x - centre.x, 0, at.z - centre.z)
      local beyond = radius + 0.9 - (rel.x * d.x + rel.z * d.z)
      local stage = Vector(at.x, 0, at.z) + d * beyond
      -- the food goes on once the piece has landed, set down on its top
      local height = config.sizes[ELEMENTS[j]].height
      spawn(p.piece[ELEMENTS[j]], {stage.x, top + 0.05, stage.z}, {0, yaw, 0}, function(piece)
        Wait.condition(function()
          local at = piece.getPosition()
          spawn(t.food, {at.x, top + height + 0.15, at.z})
        end, function() return piece.isDestroyed() or piece.resting end)
      end)
    end
    local out = Vector(centre.x, 0, centre.z) + d * (radius + 3.6)
    for j, kind in ipairs(ELEMENTS) do
      local at = out + side * ((j - 2) * 2.4)
      spawn(p.bag[kind], {at.x, top + 0.5, at.z}, {0, yaw, 0})
    end
    local back = out + d * 2.8
    spawn(p.tokens, {back.x, top + 0.5, back.z}, {0, yaw, 0})
    -- power levels in a column beside the power board
    local out_dir = (power - centre):normalized()
    local at = power + along * 4.4 + out_dir * ((i - (n + 1) / 2) * 1.1)
    for k = 1, 3 do
      spawn(p.power, {at.x, top + 0.3 * k, at.z})
    end
  end
end

function setup(n)
  local key = config.plans[tostring(n)]
  local b = config.boards[key]
  clear()
  local flip = (b.face == "back") and 180 or 0
  spawn(config.templates.boards[b.object], {0, tableTop() + 0.3, 0}, {0, 0, flip}, function(board)
    Wait.condition(function()
      board.setLock(true)
      state = {board = key, guid = board.getGUID(), players = n}
      setSnaps(board, b)
      placePlayers(board, b, n)
      broadcastToAll("ORGANISM set up for " .. n .. " players", {0.9, 0.95, 1})
    end, function() return board.resting end)
  end)
end

-- ── Events ────────────────────────────────────────────────────────────────

function setupGame(player, value, id)
  setup(tonumber(string.match(id, "%d+")))
end

function toggleSetup(player, value, id)
  local shown = UI.getAttribute("setupButtons", "active") ~= "false"
  UI.setAttribute("setupButtons", "active", shown and "false" or "true")
end

function onObjectLeaveContainer(container, object)
  fitSize(object)
end

function onLoad(saved)
  config = JSON.decode(CONFIG_JSON)
  if saved and saved ~= "" then state = JSON.decode(saved) or {} end
  for _, o in ipairs(getObjects()) do fitSize(o) end
  if state.guid then
    local board = getObjectFromGUID(state.guid)
    if board then
      whenLoaded(board, function(o) setSnaps(o, config.boards[state.board]) end)
    end
  end
end

function onSave()
  return JSON.encode(state)
end
