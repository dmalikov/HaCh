# HaCh #

## What is it? ##
**HaCh** is a simple **Ch**at written in **Ha**skell. It consists of 3 separate programs: 

*    `hach-server`  

*    `hach-client`  

*    `hach-nclient`  

It uses [`Chan`s]() via sockets for message broadcasting and [vty](http://hackage.haskell.org/package/vty) terminal GUI library for a client interface. 
  
## What it was written for? ##
It was written as an example of solving the following specification:

    Write a command-line program to send a message from one client to another
    using the server. The client should be able to send messages with some specific
    code, server should resend these messages like a messages from a third person.

## How to install? ##

`cabal install`

By default this will install 3 executables files in `~/.cabal/bin/` dir. Prefix could be set by `--prefix` option.

## How to use? ##

Run `hach-client` on the server side.

Run `hach-client -s SERVER_IP -n NICK` to join chat via simple console client or `hach-nclient -s SERVER_IP -n NICK` for [`vty`](http://hackage.haskell.org/package/vty)-client.
