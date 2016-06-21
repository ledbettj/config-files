
hs.application = require "hs.application"
hs.window      = require "hs.window"
hs.hotkey      = require "hs.hotkey"
hs.fnutils     = require "hs.fnutils"
hs.geometry    = require "hs.geometry"
hs.screen      = require "hs.screen"
hs.grid        = require "hs.grid"

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

-- find the main window belonging to the application with title 'title'
function winFromTitle(title)
   local apps = hs.application.runningApplications()
   for _, app in pairs(apps) do
      if app:title() == title then
         return app:mainWindow()
      end
   end

   return nil
end

-- return a function that moves the front window to a grid position.
function moveToGrid(grid)
   return function()
      hs.grid.set(hs.window.frontmostWindow(), grid)
   end
end

-- move the given window to the given screen, keeping the same relative
-- dimensions and placement.
function winToScreen(win, screen)
   if not win or not screen then
      return false
   end

   local old_screen       = win:screen()
   local old_screen_frame = old_screen:frame()
   local win_frame        = win:frame()
   local new_screen       = screen
   local new_frame        = new_screen:frame()
   local off_x_pct        = (win_frame.x - old_screen_frame.x) / (old_screen_frame.w)
   local off_y_pct        = (win_frame.y - old_screen_frame.y) / (old_screen_frame.h)

   local w_pct = win_frame.w / old_screen_frame.w
   local h_pct = win_frame.h / old_screen_frame.h

   win_frame.x = new_frame.x + (off_x_pct * new_frame.w)
   win_frame.y = new_frame.y + (off_y_pct * new_frame.h)
   win_frame.w = w_pct * new_frame.w
   win_frame.h = h_pct * new_frame.h

   win:setFrame(win_frame)
end

hs.hotkey.bind({"ctrl", "alt"}, "pad*",
   function()
      local screens = hs.screen.allScreens()
      local apps = {
         ["Google Chrome"] = {TOP_RIGHT, screens[1]},
         ["Emacs"]         = {FULL_LEFT, screens[1]},
         ["iTerm2"]        = {BOT_RIGHT, screens[1]},
         ["Slack"]         = {FULL_FULL, screens[2]}
      }

      for name, pos in pairs(apps) do
         local w = winFromTitle(name)
         if w then
            hs.grid.set(w, pos[1], pos[2])
         end
      end
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad7", moveToGrid(TOP_LEFT))
hs.hotkey.bind({"ctrl", "alt"}, "pad8", moveToGrid(TOP_FULL))
hs.hotkey.bind({"ctrl", "alt"}, "pad9", moveToGrid(TOP_RIGHT))
hs.hotkey.bind({"ctrl", "alt"}, "pad4", moveToGrid(FULL_LEFT))
hs.hotkey.bind({"ctrl", "alt"}, "pad5", moveToGrid(FULL_FULL))
hs.hotkey.bind({"ctrl", "alt"}, "pad6", moveToGrid(FULL_RIGHT))
hs.hotkey.bind({"ctrl", "alt"}, "pad1", moveToGrid(BOT_LEFT))
hs.hotkey.bind({"ctrl", "alt"}, "pad2", moveToGrid(BOT_FULL))
hs.hotkey.bind({"ctrl", "alt"}, "pad3", moveToGrid(BOT_RIGHT))
hs.hotkey.bind({"ctrl", "alt"}, "pad+", hs.grid.show)
hs.hotkey.bind({"ctrl", "alt"}, "padenter",
   function()
      local win = hs.window.frontmostWindow()
      winToScreen(win, win:screen():next())
   end
)

