# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of SConsErlang.
#
# SConsErlang is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# SConsErlang is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SConsErlang; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

import os
# Configuration.
options = Options("options.cache")
options.AddOptions(
    PathOption("PREFIX", "Base prefix", "/usr/local"),
    PathOption("SCONSPREFIX", "SCons prefix directory (where SCons is installed)", os.environ.get('PYTHON_ROOT',"$PREFIX/lib/scons/")),
    PathOption("ERLANGPREFIX", "Erlang prefix directory (where Erlang is installed)", "$PREFIX/lib/erlang"))

# Create an environment.
env = Environment(tools = ["default", "erlang"], toolpath = ["./"], options=options)

# Save the options.
options.Save(options.files[0], env)


# Help.
Help(options.GenerateHelpText(env))

# Compile the erlangscanner.
beams = env.Erlang("src/erlangscanner.erl")

# Install erlang.py
env.Install("$SCONSPREFIX/SCons/Tool/", "erlang.py")
env.Install("$ERLANGPREFIX/lib/sconserlang-0.1.0/ebin/", beams)
env.Install("$ERLANGPREFIX/lib/sconserlang-0.1.0/ebin/", "ebin/sconserlang.app")

# Alias for installing.
env.Alias("install", "$SCONSPREFIX")
env.Alias("install", "$ERLANGPREFIX")
