local wezterm = require 'wezterm'

local config = {}

config.font = wezterm.font 'FiraCode Nerd Font Mono'
config.term = 'wezterm'
config.font_size = 15.0
config.window_decorations = 'RESIZE'
config.enable_tab_bar = false

config.colors = {
  foreground = '#e4e4e4',
  background = '#121212',
  ansi = {
    '#121212',
    '#cc0000',
    '#4e9a06',
    '#c4a000',
    '#3465a4',
    '#75507b',
    '#06989a',
    '#d3d7cf',
  },
  brights = {
    '#555753',
    '#ef2929',
    '#8ae234',
    '#fce94f',
    '#729fcf',
    '#ad7fa8',
    '#34e2e2',
    '#eeeeec',
  }
}

return config
