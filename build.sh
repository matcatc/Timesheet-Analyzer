#!/bin/sh
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

mkdir -p _shake
# Note: ghc will only rebuild the binary if ShakeBuild.hs has changed (or it
# hasn't been built previously).
ghc --make ShakeBuild.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"

