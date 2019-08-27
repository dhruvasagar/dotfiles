# Talk : Writng a VIM Plugin

## Outline

* Who am I ?
* What is a Plugin ?
* Convert snippets of code from vimrc to a plugin
* VIM Plugin Architecture
* Advanced plugin development : Tips & Tricks
* Tools for plugin development
* References

                         © Dhruva Sagar - Tarka Labs
---
# Introduction

I am Dhruva Sagar (DS)

* Email   : dhruva [dot] sagar [at] gmail [dot] com, [^email]
* Github  : https://github.com/dhruvasagar, [^github]
* Medium  : https://medium.com/@dhruvasagar, [^medium]
* Twitter : https://twitter.com/dhruvasagar, [^twitter]

[^email]: https://dsis.me/email
[^github]: https://dsis.me/github
[^medium]: https://dsis.me/medium
[^twitter]: https://dsis.me/twitter

> NOTE: [URL Mapper](https://github.com/dhruvasagar/url-mapper)

                         © Dhruva Sagar - Tarka Labs
---
# What is a Plugin ?

A plugin is a software that can add / modify functionality of an existing
software typical using an API.

Software, even if open source can be fairly complex and may not be
easy to contribute to. A plugin architecture lowers the entry barrier for
developers to be able to contribute and improve functionality of the software.
It enables the consumers / users of the software to be able to change behavior
to suit their preferences as well as share that with other fellow developers.

                         © Dhruva Sagar - Tarka Labs
---
## What is a VIM Plugin ?

VIM is unique because it offers a programmatic approach to text editing,
a language with which you can describe text editing. VIM plugins allow us to
not only use that language for automation of more complex tasks, but to also
extend that language to newer paradigms.

> NOTE: The entry barrier for developing VIM plugins is really low, if you
> have edited your vimrc, you are ready to write vim plugins

                         © Dhruva Sagar - Tarka Labs
---
# Convert snippets of code from vimrc to a plugin

Few Examples :

* Move functions & related mappings / commands to `$VIM/plugin`
* Move filetype autocmds to ftplugins

                         © Dhruva Sagar - Tarka Labs
---
# VIM Plugin Architecture

* Plugin Management
* `runtimepath` - `:h 'runtimepath'`
  * plugin/
  * ftplugin/
  * autoload/
  * colors/
  * compiler/
  * doc/
  * indent/
  * syntax/
* `after`

                         © Dhruva Sagar - Tarka Labs
---
# Advanced plugin development : Tips & Tricks

* variable / function scopes
* Add help doc
* `:h :exec`
* `:h call()`
* `:h :normal!`
* `:h using-<Plug>`

                         © Dhruva Sagar - Tarka Labs
---
# Tools for Plugin Development

* `:echom`
* `:breakadd / :breakdel`
* tpope/vim-scriptease

                         © Dhruva Sagar - Tarka Labs
---
# References

* `:h write-plugin`
* `:h functions`

                         © Dhruva Sagar - Tarka Labs
