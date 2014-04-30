setl foldlevel=1
if expand('%') =~# '_test\.rb$'
  compiler rubyunit
  setl makeprg=testrb\ \"%:p\"
elseif expand('%') =~# '_spec\.rb$'
  compiler rspec
  setl makeprg=rspec\ \"%:p\"
else
  compiler ruby
  setl makeprg=ruby\ -wc\ \"%:p\"
endif
