# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of SConsErlang.
#
# SConsErlang is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# SConsErlang is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SConsErlang; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

from SCons.Builder import Builder
import os

def generate(env):
    env["ERLC"] = env.Detect("erlc") or "erlc"

    def addBeamTarget(target, source, env):
        newTarget = os.path.splitext(str(source[0]))[0]+".beam"
        if env.has_key("OUTPUT"):
            newTarget = env["OUTPUT"] + "/" + os.path.basename(newTarget)
        return ([newTarget], source)

    def erlangGenerator(source, target, env, for_signature):
        command = "$ERLC"
        if env.has_key("OUTPUT"):
            command += " -o " + env["OUTPUT"]
        if env.has_key("LIBPATH"):
            if not isinstance(env["LIBPATH"], list):
                env["LIBPATH"] = [env["LIBPATH"]]
            for libpath in env["LIBPATH"]:
                command += " -I " + libpath
        return command + " " + str(source[0])
    
    erlangBuilder = Builder(generator = erlangGenerator,
                            #action = "$ERLC -o $OUTPUT $SOURCE",
                            suffix = ".beam",
                            src_suffix = ".erl",
                            emitter = addBeamTarget,
                            single_source = True)
    env.Append(BUILDERS = {"Erlang" : erlangBuilder})
    env.Append(ENV = {"HOME" : os.environ["HOME"]})

def exists(env):
    return env.Detect(["erlc"])
