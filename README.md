Live
====

Live is an automated module and configuration reloader.

Usage
-----

Live is a development tool for live reloading of modules
and configuration files designed to work with the erlang.mk
and relx build tools. It allows you to add live reloading
to a release.

Live monitors file changes based on configurable rules.
By default it monitors all BEAM files found in `lib/*/ebin/*.beam`
in the release. It can detect and load new files, reload
existing files or unload deleted files.

Live can be configured to work with any kind of files in
the release, as each filename match rule has an associated
handler module which you can create by implementing the
`live_handler` behavior.

This project is a work in progress. The interface to configure
additional rules is currently missing.

Getting help
------------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
