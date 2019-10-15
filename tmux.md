# Talk : Tmuxification

## Outline

* What is Tmux and why use it for development ?
* Installing Tmux
* Tmux Configuration
* Tmux Sessions
* Windows & Panes
* Advanced Usage Patterns
* Questions

                         © Dhruva Sagar - Tarka Labs
---
# What is tmux ?

Tmux is an open source terminal multiplexer that enables you to create
multiple terminal window layouts easily and navigate through them.

Screen is another alternative, however tmux gained more popularity being more
configurable.

Tmux has a client-server model which allows you to connect and disconnect to
it as needed while keeping things running persistently, this is especially
useful on servers and you don't have to use `nohup`.

This can be used for remote pair programming on a server by connecting to the
same session.

# Why use it ?

Being portable, it is available on any platform, eg.) raspberry pi

When you ssh to a server, you can avoid sshing multiple times to the server
when you want to do multiple things and just create new windows / panes in
a tmux session instead, it's much faster and convenient.

                         © Dhruva Sagar - Tarka Labs
---
# Installing tmux

`tmux` is available on most platforms through the available package managers.

On OSX :

```sh
$ brew install tmux
```

On Ubuntu :
```sh
$ apt install tmux
```
                         © Dhruva Sagar - Tarka Labs
---
# Tmux Configuration

`tmux` keeps configuration in a `~/.tmux.conf` file by default.

This makes tmux really portable, since all you need to do is install tmux and
copy your configurations on the server / machine to reproduce the exact same
environment as per your liking

                         © Dhruva Sagar - Tarka Labs
---
# Tmux Sessions

## Creating a session

```sh
$ tmux new -s session-name
```

## Listing sessions

```sh
$ tmux ls
tunecore: 3 windows (created Mon Oct  7 07:00:32 2019)
vim: 4 windows (created Tue Oct  8 10:28:05 2019) (attached)
```

## Attaching to an existing session

```sh
$ tmux attach -t tunecore
```

## Switching sessions

```sh
$ tmux choose-session
```
                         © Dhruva Sagar - Tarka Labs
---
# Windows & Panes

Windows are similar to tabs, you can create a number of windows and they are
listed in the tmux statusline at the bottom

You can navigate between windows in a number of ways :

`<Prefix> + <number>` takes you to the window with the specified number
`<Prefix> + n` takes you to the next window
`<Prefix> + p` takes you to the previous window
`<Prefix> + l` takes you to the last window (can be used to swap between two windows quickly)

Panes are terminals in splits within the same window

You can navigate between panes in a number of ways :

`<Prefix> + o` cycle between panes
`<Prefix> + <Arrow>` takes you to the pane in the direction of the arrow

                         © Dhruva Sagar - Tarka Labs
---
# Advanced Usage Patterns

* Send text between panes
* Zoom panes
* Join / Send panes
* Sticky windows / panes

                         © Dhruva Sagar - Tarka Labs
---
# Questions

```
  /$$$$$$   /$$$      /$$$$$$
 /$$__  $$ /$$ $$    /$$__  $$
| $$  \ $$|  $$$    | $$  \ $$
| $$  | $$ /$$ $$/$$| $$$$$$$$
| $$  | $$| $$  $$_/| $$__  $$
| $$/$$ $$| $$\  $$ | $$  | $$
|  $$$$$$/|  $$$$/$$| $$  | $$
 \____ $$$ \____/\_/|__/  |__/
      \__/
```

                         © Dhruva Sagar - Tarka Labs
