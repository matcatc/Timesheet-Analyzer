#!/usr/bin/make -f
#
# Makefile for timesheet analyzer. Mainly created so we can create Debian packages.
#
# Author: Matthew Todd
# Date: Sep 22, 2013
#
#
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


