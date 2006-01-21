# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of SConsErlang.
#
# SConsErlang is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# SConsErlang is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SConsErlang; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

import os

# Create an environment.
env = Environment(tools = ["default", "erlang"], toolpath = ["./"])

# Configuration.
configFile = ".SConsErlang.conf"
opts = Options(configFile)
opts.Add(PathOption("SCONSPREFIX", "SCons prefix directory (where SCons is installed)", os.environ.get('PYTHON_ROOT',"/usr/local/lib/scons/")))
opts.Add(PathOption("ERLANGPREFIX", "Erlang prefix directory (where Erlang is installed)", "/usr/local/lib/erlang"))
opts.Update(env)
opts.Save(configFile, env)

# Help.
Help(opts.GenerateHelpText(env))

# Compile the erlangscanner.
beams = env.Erlang("erlangscanner.erl")

# Install erlang.py
env.Install("$SCONSPREFIX/SCons/Tool/", "erlang.py")
env.Install("$ERLANGPREFIX/lib/sconserlang-0.0.0/ebin/", beams)
env.Install("$ERLANGPREFIX/lib/sconserlang-0.0.0/ebin/", "sconserlang.app")

# Alias for installing.
env.Alias("install", "$SCONSPREFIX")
env.Alias("install", "$ERLANGPREFIX")
