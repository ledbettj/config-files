hs.window.animationDuration = 0 -- animations are dumb. move my windows fast.
hs.grid.setGrid('4x4')

local TOP_LEFT   = "0,0,2,2"
local TOP_FULL   = "0,0,4,2"
local TOP_RIGHT  = "2,0,4,2"
local FULL_LEFT  = "0,0,2,4"
local FULL_FULL  = "0,0,4,4"
local FULL_RIGHT = "2,0,4,4"
local BOT_LEFT   = "0,2,2,4"
local BOT_FULL   = "0,2,4,4"
local BOT_RIGHT  = "2,2,4,4"

-- return a function that moves the front window to a grid position,
-- and focuses it if it's not focused.
function moveToGrid(grid)
   return function()
      local win = hs.window.frontmostWindow()
      hs.grid.set(win, grid)
      if not hs.window.focusedWindow() then
         win:focus()
      end
   end
end

hs.hotkey.bind({"ctrl", "cmd"}, "pad*",
   function()
      local screens = hs.screen.allScreens()
      local apps = {
         ["Google Chrome"] = {TOP_RIGHT, screens[1]},
         ["Emacs"]         = {FULL_LEFT, screens[1]},
         ["iTerm2"]        = {BOT_RIGHT, screens[1]},
         ["Slack"]         = {FULL_FULL, screens[2]}
      }

      for name, pos in pairs(apps) do
         local app = hs.application.find(name)
         local win = nil
         if app then
            hs.grid.set(app:mainWindow(), pos[1], pos[2])
         end
      end
   end
)

-- pass in a function to change the currently focused window.
-- after the focus is changed, we'll draw a brief rect around the window
-- to highlight it.
function setFocus(fn)
   return function()
      fn()
      local win = hs.window.focusedWindow()
      if win then
         local rect = hs.drawing.rectangle(win:frame())
         rect:setFill(false)
         rect:setStroke(true)
         rect:setStrokeWidth(5)
         rect:setStrokeColor(hs.drawing.color.osx_red)
         rect:setLevel(hs.drawing.windowLevels["floating"])
         rect:setRoundedRectRadii(4, 4)
         rect:show()
         rect:hide(0.65)
         hs.timer.doAfter(0.65, function() rect:delete() end)
      end
   end
end

hs.hotkey.bind({"ctrl", "cmd"}, "left",  setFocus(hs.window.filter.focusWest))
hs.hotkey.bind({"ctrl", "cmd"}, "right", setFocus(hs.window.filter.focusEast))
hs.hotkey.bind({"ctrl", "cmd"}, "up",    setFocus(hs.window.filter.focusNorth))
hs.hotkey.bind({"ctrl", "cmd"}, "down",  setFocus(hs.window.filter.focusSouth))

-- window movement
hs.hotkey.bind({"ctrl", "cmd"}, "pad7", moveToGrid(TOP_LEFT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad8", moveToGrid(TOP_FULL))
hs.hotkey.bind({"ctrl", "cmd"}, "pad9", moveToGrid(TOP_RIGHT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad4", moveToGrid(FULL_LEFT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad5", moveToGrid(FULL_FULL))
hs.hotkey.bind({"ctrl", "cmd"}, "pad6", moveToGrid(FULL_RIGHT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad1", moveToGrid(BOT_LEFT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad2", moveToGrid(BOT_FULL))
hs.hotkey.bind({"ctrl", "cmd"}, "pad3", moveToGrid(BOT_RIGHT))
hs.hotkey.bind({"ctrl", "cmd"}, "pad+", hs.grid.show)
hs.hotkey.bind({"ctrl", "cmd"}, "padenter",
   function()
      local win = hs.window.frontmostWindow()
      local screen = win:screen():next()
      win:moveToScreen(screen)
   end
)

