
hs.application = require "hs.application"
hs.window      = require "hs.window"
hs.hotkey      = require "hs.hotkey"
hs.fnutils     = require "hs.fnutils"
hs.geometry    = require "hs.geometry"
hs.screen      = require "hs.screen"
hs.grid        = require "hs.grid"

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

-- move the given window to the position ('topleft', 'topright', etc),
-- also moving it to the provided screen if any.
function move(win, where, screen)
   if not win or not where then
      return false
   end
   winToScreen(win, screen)
   _G[where](win)
end

function topleft(win)
   win:moveToUnit(hs.geometry.rect(0, 0, 0.5, 0.5))
end

function top(win)
   win:moveToUnit(hs.geometry.rect(0, 0, 1, 0.5))
end

function topright(win)
   win:moveToUnit(hs.geometry.rect(0.5, 0, 0.5, 0.5))
end

function left(win)
   win:moveToUnit(hs.geometry.rect(0, 0, 0.5, 1))
end

function full(win)
   win:moveToUnit(hs.geometry.rect(0, 0, 1, 1))
end

function right(win)
   win:moveToUnit(hs.geometry.rect(0.5, 0, 0.5, 1))
end

function bottomleft(win)
   win:moveToUnit(hs.geometry.rect(0, 0.5, 0.5, 0.5))
end

function bottom(win)
   win:moveToUnit(hs.geometry.rect(0, 0.5, 1, 0.5))
end

function bottomright(win)
   win:moveToUnit(hs.geometry.rect(0.5, 0.5, 0.5, 0.5))
end

hs.hotkey.bind({"ctrl", "alt"}, "pad*",
   function()
      local screens = hs.screen.allScreens()
      local apps = {
         ["Google Chrome"] = {"topright", screens[1]},
         ["Emacs"]         = {"left",     screens[1]},
         ["iTerm2"]        = {"bottomright", screens[1]},
         ["HipChat"]       = {"left", screens[2]},
         ["Slack"]         = {"full", screens[2]},
         ["Mail"]          = {"right",  screens[2]},
         ["Mailbox (Beta)"]= {"right",  screens[2]}
      }

      for name, pos in pairs(apps) do
         local w = winFromTitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad7",
   function()
      topleft(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad8",
   function()
      top(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad9",
   function()
      topright(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad4",
   function()
      left(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad5",
   function()
      full(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad6",
   function()
      right(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad1",
   function()
      bottomleft(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad2",
   function()
      bottom(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "pad3",
   function()
      bottomright(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "padenter",
   function()
      local win = hs.window.focusedWindow()
      winToScreen(win, win:screen():next())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "left",
   function()
      left(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "right",
   function()
      right(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "up",
   function()
      top(hs.window.focusedWindow())
   end
)
hs.hotkey.bind({"ctrl", "alt"}, "down",
   function()
      bottom(hs.window.focusedWindow())
   end
)

hs.hotkey.bind({"ctrl", "alt"}, "return",
   function()
      full(hs.window.focusedWindow())
   end
)
