---Textobject for adjacent commented lines
local function commented_lines_textobject()
  local U = require("Comment.utils")
  local cl = vim.api.nvim_win_get_cursor(0)[1] -- current line
  local range = { srow = cl, scol = 0, erow = cl, ecol = 0 }
  local ctx = {
    ctype = U.ctype.linewise,
    range = range,
  }
  local cstr = require("Comment.ft").calculate(ctx) or vim.bo.commentstring
  local ll, rr = U.unwrap_cstr(cstr)
  local padding = true
  local is_commented = U.is_commented(ll, rr, padding)

  local line = vim.api.nvim_buf_get_lines(0, cl - 1, cl, false)
  if next(line) == nil or not is_commented(line[1]) then
    return
  end

  local rs, re = cl, cl -- range start and end
  repeat
    rs = rs - 1
    line = vim.api.nvim_buf_get_lines(0, rs - 1, rs, false)
  until next(line) == nil or not is_commented(line[1])
  rs = rs + 1
  repeat
    re = re + 1
    line = vim.api.nvim_buf_get_lines(0, re - 1, re, false)
  until next(line) == nil or not is_commented(line[1])
  re = re - 1

  vim.fn.execute("normal! " .. rs .. "GV" .. re .. "G")
end

vim.keymap.set(
  "o",
  "gc",
  commented_lines_textobject,
  { silent = true, desc = "Textobject for adjacent commented lines" }
)
