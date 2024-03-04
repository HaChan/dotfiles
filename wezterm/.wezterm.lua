-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux
local act = wezterm.action

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
else
  config = {}
end

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

local is_linux = wezterm.target_triple == "x86_64-unknown-linux-gnu"
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"

-- This is where you actually apply your config choices
local wsl_domains = wezterm.default_wsl_domains()

for _, dom in ipairs(wsl_domains) do
  if dom.name == 'WSL:Ubuntu2' then
    dom.default_cwd = "/home/hachan"
  end
end


config.launch_menu = (is_windows and {
  {
    args = { "cmd.exe" },
    domain = {
      DomainName = "local"
    }
  }
}) or (is_linux and {
  {
    label = "Bash Login Shell",
    args = { "/bin/bash", "--login" },
    domain = { DomainName = "local" }
  }
}) or {}


config.set_environment_variables = {
  TERMINFO_DIRS = "/home/" .. (os.getenv("USERNAME") or os.getenv("USER")) .. "/.nix-profile/share/terminfo",
  WSLENV = "TERMINFO_DIRS",
  prompt = is_windows and "$E]7;file://localhost/$P$E\\$E[32m$T$E[0m $E[35m$P$E[36m$_$G$E[0m " or nil,
}

-- config.default_domain = 'WSL:Ubuntu2'
-- config.wsl_domains = wsl_domains
-- config.default_prog = { "wsl.exe", "~" }
-- config.default_prog = {"/bin/bash", "--login"}
-- config.default_cwd = "/home/hachan"

-- For example, changing the color scheme:
config.color_scheme = 'Dracula'

config.tab_bar_at_bottom = true
-- config.use_fancy_tab_bar = false
config.window_decorations = "RESIZE"
-- config.hide_tab_bar_if_only_one_tab = true

config.leader = { key="m", mods="CTRL" }
config.keys = {
  {
    key = 'E',
    mods = 'CTRL|SHIFT',
    action = act.PromptInputLine {
      description = 'Enter new name for tab',
      action = wezterm.action_callback(function(window, pane, line)
        -- line will be `nil` if they hit escape without entering anything
        -- An empty string if they just hit enter
        -- Or the actual line of text they wrote
        if line then
          window:active_tab():set_title(line)
        end
      end),
    },
  },
  { key = "l", mods = "LEADER|CTRL", action = act.ActivateTabRelative(1) },
  { key = "h", mods = "LEADER|CTRL", action = act.ActivateTabRelative(-1) },
  { key = "u", mods = "LEADER|CTRL", action = act.ScrollByPage(-0.5) },
  { key = "d", mods = "LEADER|CTRL", action = act.ScrollByPage(0.5) },
  { key = "UpArrow", mods = "SHIFT", action = act.ScrollByLine(-1) },
  { key = "DownArrow", mods = "SHIFT", action = act.ScrollByLine(1) },
  { key = "b", mods = "LEADER|CTRL",  action=wezterm.action{SendString="\x02"}},
  { key = "-", mods = "LEADER",       action=wezterm.action{SplitVertical={ domain="CurrentPaneDomain", cwd = '~', } } },
  { key = "\\",mods = "LEADER",       action=wezterm.action{SplitHorizontal={ domain="CurrentPaneDomain", cwd = '~', } } },
  { key = "s", mods = "LEADER",       action=wezterm.action{SplitVertical={ domain="CurrentPaneDomain", cwd = '~', } } },
  { key = "v", mods = "LEADER",       action=wezterm.action{SplitHorizontal={ domain="CurrentPaneDomain", cwd = '~', } } },
  { key = "o", mods = "LEADER",       action="TogglePaneZoomState" },
  { key = "z", mods = "LEADER",       action="TogglePaneZoomState" },
  { key = "c", mods = "LEADER",       action=wezterm.action{SpawnCommandInNewTab={ domain="CurrentPaneDomain", cwd = '~', } } },
  { key = "h", mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Left"}},
  { key = "j", mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Down"}},
  { key = "k", mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Up"}},
  { key = "l", mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Right"}},
  { key = "H", mods = "LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Left", 5}}},
  { key = "J", mods = "LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Down", 5}}},
  { key = "K", mods = "LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Up", 5}}},
  { key = "L", mods = "LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Right", 5}}},
  { key = "1", mods = "LEADER",       action=wezterm.action{ActivateTab=0}},
  { key = "2", mods = "LEADER",       action=wezterm.action{ActivateTab=1}},
  { key = "3", mods = "LEADER",       action=wezterm.action{ActivateTab=2}},
  { key = "4", mods = "LEADER",       action=wezterm.action{ActivateTab=3}},
  { key = "5", mods = "LEADER",       action=wezterm.action{ActivateTab=4}},
  { key = "6", mods = "LEADER",       action=wezterm.action{ActivateTab=5}},
  { key = "7", mods = "LEADER",       action=wezterm.action{ActivateTab=6}},
  { key = "8", mods = "LEADER",       action=wezterm.action{ActivateTab=7}},
  { key = "9", mods = "LEADER",       action=wezterm.action{ActivateTab=8}},
  { key = "W", mods = "LEADER|SHIFT", action=wezterm.action{CloseCurrentTab={confirm=true}}},
  { key = "d", mods = "LEADER",       action=wezterm.action{CloseCurrentPane={confirm=true}}},
  { key = "x", mods = "LEADER",       action=wezterm.action{CloseCurrentPane={confirm=true}}},
}

-- and finally, return the configuration to wezterm
return config
