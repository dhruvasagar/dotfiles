let g:ruby_debugger_no_maps = 1
let g:ruby_debugger_fast_sender = 1
let g:ruby_debugger_executable = "bundle exec rdebug-vim"

nnoremap <F5> :call g:RubyDebugger.step()<CR>
nnoremap <F6> :call g:RubyDebugger.next()<CR>
nnoremap <F7> :call g:RubyDebugger.finish()<CR>
nnoremap <F8> :call g:RubyDebugger.continue()<CR>

nnoremap <Leader>db  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.toggle_breakpoint()<CR>
nnoremap <Leader>dv  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.open_variables()<CR>
nnoremap <Leader>dm  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.open_breakpoints()<CR>
nnoremap <Leader>dt  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.open_frames()<CR>
nnoremap <Leader>ds  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.step()<CR>
nnoremap <Leader>df  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.finish()<CR>
nnoremap <Leader>dn  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.next()<CR>
nnoremap <Leader>dc  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.continue()<CR>
nnoremap <Leader>de  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.exit()<CR>
nnoremap <Leader>dd  :call ruby_debugger#load_debugger() <Bar> call g:RubyDebugger.remove_breakpoints()<CR>
