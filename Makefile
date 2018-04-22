#!/usr/bin/make -f
#
# Makefile for timesheet analyzer. Mainly created so we can create Debian packages.
#
# Author: Matthew Todd
# Date: Sep 22, 2013
#
# Copyright 2018 Matthew Todd
# 
# This file is part of Timesheet Analyzer.
# 
# Timesheet Analyzer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
# 
# Timesheet Analyzer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# Timesheet Analyzer.  If not, see <http://www.gnu.org/licenses/>.


# TODO: rewrite to use pkg-config?

# TODO: once done debugging, delete this. (Delete for 1.0 release.)
# Added so we don't accidentally install onto the real system.
DESTDIR = /tmp/timesheet_analyzer_test/


prefix = /usr/local
bindir = $(prefix)/bin
sharedir = $(prefix)/share
etcdir = /etc/boring-todo/
docdir = $(prefix)/share/doc/timesheet-analyzer/
bashcompletiondir = $(prefix)/share/bash-completion/completions/
manpagedir = $(sharedir)/man/
haddockdir = $(docdir)/haddock/


# we use Shake as our "backend", and it builds the program, the README, and the
# haddock docs.
.PHONY: all
all:
	./build.sh



# TODO: do all of these need to be here?
#  Ie: the doc(s)?
.PHONY: install
install: all
#	TODO: fill in (following is example from boring todo)
#	install -D _build/boring_todo $(DESTDIR)$(bindir)/boring_todo
#	install -D boring_todo.ini $(DESTDIR)$(etcdir)/boring_todo.ini
#	install -D todo_template.todo $(DESTDIR)$(etcdir)/todo_template.todo
#	install -D _build/README.html $(DESTDIR)$(docdir)/README.html
#	install -D _build/UserManual.html $(DESTDIR)$(docdir)/UserManual.html
#	install -D _build/Planning.html $(DESTDIR)$(docdir)/Planning.html
#	install -D _build/TechnicalManual.html $(DESTDIR)$(docdir)/TechnicalManual.html
#	install -D _build/boring_todo.1 $(DESTDIR)$(manpagedir)/man1/boring_todo.1
#	install -d $(DESTDIR)$(haddockdir)
#	install -t $(DESTDIR)$(haddockdir) _build/haddock/* 
#	install -D _build/boring_todo.comp $(DESTDIR)$(bashcompletiondir)/boring_todo


# Used for cleaning the built files prior to rolling a release.
#  See the README for more info
# I'm trying to avoid using any kind of globbing (*) or finding to avoid
#  accidentally deleting files. As such, there are commands that individually
#  delete files in a directory one a time.
.PHONY: clean
clean:
	# clean _build
	./build.sh clean
	# clean _shake?
	rm -f _shake/build
	rm -f _shake/Main.hi
	rm -f _shake/Main.o
	rmdir _shake
	# my vim configuration will sometimes create the below files, so removing them
	rm -f .hdevtools.sock


