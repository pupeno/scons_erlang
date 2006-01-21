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

    def addTarget(target, source, env):
        """ Adds the targets (.beam, .script and/or .boot) according to source's extension, source's path and $OUTPUT. """

        # Tear appart the source.
        filename = os.path.basename(str(source[0]))
        extension = os.path.splitext(filename)[1]
        basename = os.path.splitext(filename)[0]
        directory = os.path.dirname(str(source[0]))
        
        # Use $OUTPUT or where the source is as the prefix.
        if env.has_key("OUTPUT"):
            prefix = env["OUTPUT"] + "/"
        elif directory != "":
            prefix = directory + "/"
        else:
            prefix = ""

        # Generate the targen according to the source.
        if extension == ".erl":
            # .erls generate a .beam.
            return ([prefix + basename + ".beam"], source)
        elif extension == ".rel":
            # .rels generate a .script and a .boot.
            return ([prefix + basename + ".script", prefix + basename + ".boot"], source) 
        else:
            return (target, source)

    def erlangGenerator(source, target, env, for_signature):
        """ Generate the erlc compilation command line. """
        source = str(source[0])
        command = "$ERLC"
        if env.has_key("OUTPUT"):
            command += " -o " + env["OUTPUT"]
        else:
            command += " -o " + os.path.dirname(source)
        if env.has_key("LIBPATH"):
            if not isinstance(env["LIBPATH"], list):
                env["LIBPATH"] = [env["LIBPATH"]]
            for libpath in env["LIBPATH"]:
                command += " -I " + libpath
        return command + " " + source
    
    erlangBuilder = Builder(generator = erlangGenerator,
                            #action = "$ERLC -o $OUTPUT $SOURCE",
                            #suffix = [".beam", ".boot", ".script"],
                            src_suffix = ".erl",
                            emitter = addTarget,
                            single_source = True)
    env.Append(BUILDERS = {"Erlang" : erlangBuilder})
    env.Append(ENV = {"HOME" : os.environ["HOME"]})

def exists(env):
    return env.Detect(["erlc"])
