# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of SConsErlang.
#
# SConsErlang is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# SConsErlang is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SConsErlang; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

from SCons.Builder import Builder
from SCons.Scanner import Scanner
import string
import os
import subprocess

def generate(env):
    env["ERLC"] = env.Detect("erlc") or "erlc"
    env["ERL"] = env.Detect("erl") or "erl"

    bugReport = "Please report it to Pupeno <pupeno@pupeno.com> (http://pupeno.com)."

    def addTarget(target, source, env):
        """ Adds the targets (.beam, .script and/or .boot) according to source's extension, source's path and $OUTPUT. """

        # We should receive one and only one source.
        if len(source) > 1:
            print "Warning: unexpected internal situation."
            print "This is a bug. %s" % bugReport
            print "addTarget received more than one source."
            print "addTarget(%s, %s, %s)" % (source, target, env)

        sourceStr = str(source[0])
        
        # Tear appart the source.
        filename = os.path.basename(sourceStr)
        extension = os.path.splitext(filename)[1]
        basename = os.path.splitext(filename)[0]
        
        # Use $OUTPUT or where the source is as the prefix.
        prefix = outputDir(sourceStr, env)

        # Generate the targen according to the source.
        if extension == ".erl":
            # .erls generate a .beam.
            return ([prefix + basename + ".beam"], source)
        elif extension == ".rel":
            # .rels generate a .script and a .boot.
            return ([prefix + basename + ".script", prefix + basename + ".boot"], source) 
        else:
            print "Warning: extension '%s' is unknown." % extension
            print "If you feel this is a valid extension, then it might be a missing feature or a bug. %s" % bugReport
            print "addTarget(%s, %s, %s)." % (target, source, env)
            return (target, source)

    def erlangGenerator(source, target, env, for_signature):
        """ Generate the erlc compilation command line. """

        # We should receive one and only one source.
        if len(source) > 1:
            print "Warning: unexpected internal situation."
            print "This is a bug. %s" % bugReport
            print "erlangGenerator received more than one source."
            print "erlangGenerator(%s, %s, %s, %s)" % (source, target, env, for_signature)
            
        source = str(source[0])

        # Start with the complier.
        command = "$ERLC"

        # The output (-o) parameter
        command += " -o " + outputDir(source, env)
        
        # Add the libpaths.
        if env.has_key("LIBPATH"):
            if not isinstance(env["LIBPATH"], list):
                env["LIBPATH"] = [env["LIBPATH"]]
            for libpath in env["LIBPATH"]:
                command += " -I " + libpath

        # At last, the source.
        return command + " " + source
    
    erlangBuilder = Builder(generator = erlangGenerator,
                            #action = "$ERLC -o $OUTPUT $SOURCE",
                            #suffix = [".beam", ".boot", ".script"],
                            src_suffix = ".erl",
                            emitter = addTarget,
                            single_source = True)
    env.Append(BUILDERS = {"Erlang" : erlangBuilder})
    env.Append(ENV = {"HOME" : os.environ["HOME"]})  # erlc needs $HOME.

    def outputDir(source, env):
        """ Given a source and its environment, return the output directory. """
        if env.has_key("OUTPUT"):
            return env["OUTPUT"]
        else:
            return dirOf(source)

    def libpath(env):
        """ Return a list of the libpath or an empty list. """
        if env.has_key("LIBPATH"):
            if isinstance(env["LIBPATH"], list):
                return env["LIBPATH"]
            else:
                return [env["LIBPATH"]]
        else:
            return []

    def dirOf(filename):
        """ Returns the relative directory of filename. """
        directory = os.path.dirname(filename)
        if directory == "":
            return "./"
        else:
            return directory + "/"

    def relModules(node, env, path):
        """ Return a list of modules needed by a release (.rel) file. """

        # Run the function reApplications of erlangscanner to get the applications.
        command = "erl -noshell -s erlangscanner relApplications \"" + str(node) + "\" -s init stop"
        sp = subprocess.Popen(command,
                              shell = True,
                              stdin = None,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE)
        sp.wait()
        if sp.returncode != 0:
            print "Warning: The scanner failed to scan your files, dependencies won't be calculated."
            print "If your file '%s' is correctly (syntactically and semantically), this is a bug. %s" % (node, bugReport)
            print "Command: %s." % command
            print "Return code: %s." % sp.returncode
            print "Output: \n%s\n" % sp.stdout.read().strip()
            print "Error: \n%s\n" % sp.stderr.read().strip()
            return []

        # Get the applications defined in the .rel.
        appNames = sp.stdout.read().split()

        # Build the search path
        paths = set([outputDir(str(node), env)] + libpath(env))

        modules = []
        for path in paths:
            for appName in appNames:
                appFileName = path + appName + ".app"
                if os.access(appFileName, os.R_OK):
                    modules += appModules(appFileName, env, path)
        return modules

    def appModules(node, env, path):
        """ Return a list of modules needed by a application (.app) file. """
        
        # Run the function appModules of erlangscanner to get the modules.
        command = "erl -noshell -s erlangscanner appModules \"" + str(node) + "\" -s init stop"
        sp = subprocess.Popen(command,
                              shell = True,
                              stdin = None,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE)
        sp.wait()
        if sp.returncode != 0:
            print "Warning: The scanner failed to scan your files, dependencies won't be calculated."
            print "If your file '%s' is correctly (syntactically and semantically), this is a bug. %s" % (node, bugReport)
            print "Command: %s." % command
            print "Return code: %s." % sp.returncode
            print "Output: \n%s\n" % sp.stdout.read().strip()
            print "Error: \n%s\n" % sp.stderr.read().strip()
            return []

        # Get the applications defined in the .rel.
        moduleNames = sp.stdout.read().split()

        # Build the search path
        paths = set([outputDir(node, env)] + libpath(env))

        modules = []
        # When there are more than one application in a project, since we are scanning all paths against all files, we might end up with more dependencies that really exists. The worst is that we'll get recompilation of a file that didn't really needed it.
        for path in paths:
            for moduleName in moduleNames:
                modules.append(moduleName + ".beam")
        return modules

    relScanner = Scanner(function = relModules,
                         name = "RelScanner",
                         skeys = [".rel"],
                         recursive = False)
    env.Append(SCANNERS = relScanner)

    def edocGenerator(source, target, env, for_signature):
        """ Generate the command line to generate the code. """
        tdir = os.path.dirname(str(target[0])) + "/"
        
        command = "erl -noshell -run edoc_run files '[%s]' '[{dir, \"%s\"}]' -run init stop" % (
            ",".join(['"' + str(x) + '"' for x in source]),
            tdir)

        return command
    
    def documentTargets(target, source, env):
        """ Artifitially create all targets that generating documentation will generate to clean them up latter. """
        tdir = os.path.dirname(str(target[0])) + "/"

        newTargets = [str(target[0])]
        # TODO: What happens if two different sources has the same name on different directories ?
        newTargets += [tdir + os.path.splitext(os.path.basename(filename))[0] + ".html"
                       for filename in map(str, source)]

        newTargets += [tdir + filename for filename in
                       ["edoc-info", "modules-frame.html", "overview-summary.html", "overview-summary.html", "stylesheet.css", "packages-frame.html"]]

        #newSources = source + [tdir + "overview.edoc"]
        return (newTargets, source)

    def edocScanner(node, env, path):
        #print "edocScanner(%s, %s, %s)\n" % (node, env, path)
        overview = os.path.dirname(str(node)) + "/overview.edoc"
        if os.path.exists(overview):
            return ["overview.edoc"]
        else:
            return []
        
    edocBuilder = Builder(generator = edocGenerator,
                          emitter = documentTargets,
                          target_scanner = Scanner(function=edocScanner))
    env.Append(BUILDERS = {"EDoc" : edocBuilder})
    
def exists(env):
    return env.Detect(["erlc"])
