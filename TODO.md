
VERSION 0.1

feature:
* Useful show command
* Syntax/Parsing/printing of rule programs


VERSION 0.2

feature:
* Use result monad
* Trend and History commands
* Autosave of world for server
* Server commands, e.g. save as certain file
* Implement challenges/opportunities. I.e Rules that are only usable
  a certain number of times
* Implement Routines, i.e. Rules that are applied periodically. (Maybe
  needs improved rulang system)
* Improve shell parsing
* Error messages for programs
* Inline operators for programs

internal:
* Give proposals and ballots names -> can address ballots via ions
* Need status system for client queries
  - right now, check for closed connection is really ugly (string equality)
* Make rows more elegant internally
* Remove counted entries, use simple tuples (id, dat)


VERSION 0.3

feature:
* Better show / search / extract functionality in drgc
* base drgc on Fmt package
* Stable Basic Web/Android app
