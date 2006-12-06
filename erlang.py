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
    bugReport = "Please report it to Pupeno <pupeno@pupeno.com> (http://pupeno.com/software/scons-erlang)."
    env["ERLC"] = env.Detect("erlc")
    env["ERL"] = env.Detect("erl")
    env["RUN_ERL"] = env.Detect("run_erl")
    env["ERL_CALL"] = env.Detect("erl_call")
    
    # The following might be useful, but it is actually needed before the environment is built, so, it is useless.
    # Find the Erlang root.
    #command = "erl -noshell -s erlangscanner erlang_dir -s init stop"
    #sp = subprocess.Popen(command,
    #                      shell = True,
    #                      stdin = None,
    #                      stdout = subprocess.PIPE,
    #                      stderr = subprocess.PIPE)
    #sp.wait()
    #if sp.returncode != 0:
    #    print("Warning: Failed to find Erlang's root directory, falling back to a default.")
    #    print("If you have a correct Erlang installation (and it seems likely if you reached this point) and you are not compiling SCons Erlang itself righ now, this is likely to be a bug. %s" % bugReport)
    #    print("Command: %s." % command)
    #    print("Return code: %s." % sp.returncode)
    #    print("Output: \n%s\n" % sp.stdout.read().strip())
    #    print("Error: \n%s\n" % sp.stderr.read().strip())
    #    env["ERLROOTDIR"] = "/usr/local/lib/erlang"
    #else:
    #    env["ERLROOTDIR"] = sp.stdout.read()
    #
    #env["ERLLIBDIR"] = "$ERLROOTDIR/lib"
    
    ##################################################
    ##### Erlang Builder methods and definitions #####

    def addTarget(target, source, env):
        """ When the source is a .erl file the target is a .beam file of the same base name and on the output directory according to the user preferences. When the source is a .rel file the targets are a .script and a .boot files with the same rules as for .erl files. For everything else just compalain.
            If the user chooses to set the output to false disabling automating handling of the output, this function will be useless.
            The goal of this function is to be used as emitter on the Erlang builder, it make SCons aware of what files to clean when cleaning. """
        
        # We should receive one and only one source.
        if len(source) > 1:
            print("Warning: unexpected internal situation.")
            print("This is a bug. %s" % bugReport)
            print("addTarget received more than one source.")
            print("addTarget(%s, %s, %s)" % (source, target, env))
        
        sourceStr = str(source[0])
        
        # Tear appart the source.
        filename = os.path.basename(sourceStr)
        extension = os.path.splitext(filename)[1]
        basename = os.path.splitext(filename)[0]
        
        # Get the directory where the output is going to end.
        output_dir = outputDir(sourceStr, env)
        
        # If the output_dir is False the user doesn't want us to do automatic handling of the output.
        if output_dir:
            # Generate the targen according to the source.
            if extension == ".erl":
                # .erls generate a .beam.
                return ([output_dir + basename + ".beam"], source)
            elif extension == ".rel":
                # .rels generate a .script and a .boot.
                return ([output_dir + basename + ".script", output_dir + basename + ".boot"], source) 
            else:
                print("Warning: extension '%s' is unknown." % extension)
                print("If you feel this is a valid extension, then it might be a missing feature or a bug. %s" % bugReport)
                print("addTarget(%s, %s, %s)." % (target, source, env))
                return (target, source)
        else:
            return (target, source)
    
    def erlangGenerator(source, target, env, for_signature):
        """ Generate the erlc compilation command line.
            The purpose of this function is to be used in the Erlang builder. """
        
        # We should receive one and only one source.
        if len(source) > 1:
            print("Warning: unexpected internal situation.")
            print("This is a bug. %s" % bugReport)
            print("erlangGenerator received more than one source.")
            print("erlangGenerator(%s, %s, %s, %s)" % (source, target, env, for_signature))
        
        # Get the source name as a string.
        source = str(source[0])
        
        # Do we have any path to prepend ?
        if env.has_key("PATHPREPEND"):
            if isinstance(env["PATHPREPEND"], str):
                path_prepend = [env["PATHPREPEND"]]
            else:
                path_prepend = env["PATH_PREPEND"]
        else:
            path_prepend = []

        # Do we have any path to append ?
        if env.has_key("PATHAPPEND"):
            if isinstance(env["PATHAPPEND"], str):
                path_append = [env["PATHAPPEND"]]
            else:
                path_append = env["PATHAPPEND"]
        else:
            path_append = []
        
        # Get the output directory to be used or False if no automatic output handling is going to be used.
        output_dir = outputDir(source, env)
        
        # Start with the complier.
        command = "$ERLC $ERLFLAGS"

        # Add the output statment if it's being used.
        if output_dir:
            command += " -o " + output_dir
            path_prepend.append(output_dir)
        
        # Path preppend.
        if path_prepend:
            arg = " -pa "
            command += arg + arg.join(path_prepend) + " "
        
        # Path append.
        if path_append:
            arg = " -pz "
            command += arg + arg.join(path_append) + " "
        
        # Add the libpaths.
        if env.has_key("LIBPATH"):
            if not isinstance(env["LIBPATH"], list):
                env["LIBPATH"] = [env["LIBPATH"]]
            for libpath in env["LIBPATH"]:
                command += " -I " + libpath
        
        # At last, the source.
        return command + " " + source
    
    erlangBuilder = Builder(generator = erlangGenerator,
                            #suffix = [".beam", ".boot", ".script"],
                            src_suffix = ".erl",
                            emitter = addTarget,
                            single_source = True)
    env.Append(BUILDERS = {"Erlang" : erlangBuilder})
    env.Append(ENV = {"HOME" : os.environ["HOME"]})  # erlc needs $HOME.

    ###############################################################
    ##### Erlang Release file scanner methods and definitions #####
    
    def erlangRelScanner(node, env, path):
        """ Erlang Release file scanner. Returns the list of modules needed by a .rel file to be compiled into the .script and .boot files. """

        # Get the name of the release file as a string.
        rel = str(node)

        # Build the search path for applications and modules.
        output_dir = outputDir(rel, env)
        if output_dir:
            paths = set([output_dir] + libpath(env))
        else:
            paths = set(libpath(env))

        # Find the applications needed by the release.
        apps = appNeededByRel(rel, paths)
        
        modules = []
        # Find the modules needed by each application.
        for app in apps:
            mods = modNeededByApp(app, paths)
            modules += mods
        return modules
    
    def appNeededByRel(rel, paths):
        """ Return a list of applications (.app files) needed by a release (.rel file). """
        
        # Run the function relApplications of erlangscanner to get the applications.
        command = "erl -noshell -s erlangscanner app_needed_by_rel \"%s\" -s init stop" % rel
        
        sp = subprocess.Popen(command,
                              shell = True,
                              stdin = None,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE)
        sp.wait()
        if sp.returncode != 0:
            print("Warning: The scanner failed to scan your files, dependencies won't be calculated.")
            print("If your file '%s' is correctly (syntactically and semantically), this is a bug. %s" % (rel, bugReport))
            print("Command: %s." % command)
            print("Return code: %s." % sp.returncode)
            print("Output: \n%s\n" % sp.stdout.read().strip())
            print("Error: \n%s\n" % sp.stderr.read().strip())
            return []
        
        # Get the applications defined in the .rel.
        appNames = sp.stdout.read().split()
        
        modules = []
        for path in paths:
            for appName in appNames:
                appFileName = path + appName + ".app"
                if os.access(appFileName, os.R_OK):
                    modules.append(appFileName)
                    #modules += appModules(appFileName, env, path)
        return modules
    
    def modNeededByApp(app, paths):
        """ Return a list of modules (.beam files) needed by a application (.app file). """
        
        # Run the function appModules of erlangscanner to get the modules.
        command = "erl -noshell -s erlangscanner mod_needed_by_app \"%s\" -s init stop" % app
        
        sp = subprocess.Popen(command,
                              shell = True,
                              stdin = None,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE)
        sp.wait()
        if sp.returncode != 0:
            print("Warning: The scanner failed to scan your files, dependencies won't be calculated.")
            print("If your file '%s' is correctly (syntactically and semantically), this is a bug. %s" % (app, bugReport))
            print("Command: %s." % command)
            print("Return code: %s." % sp.returncode)
            print("Output: \n%s\n" % sp.stdout.read().strip())
            print("Error: \n%s\n" % sp.stderr.read().strip())
            return []
        
        # Get the applications defined in the .rel.
        moduleNames = sp.stdout.read().split()
        
        modules = []
        # When there are more than one application in a project, since we are scanning all paths against all files, we might end up with more dependencies that really exists. The worst is that we'll get recompilation of a file that didn't really needed it.
        for path in paths:
            for moduleName in moduleNames:
                modules.append(os.path.abspath(path + moduleName +".beam"))
        return modules
    
    erlangRelScanner = Scanner(function = erlangRelScanner,
                               name = "ErlangRelScanner",
                               skeys = [".rel"],
                               recursive = False)
    env.Append(SCANNERS = erlangRelScanner)

    #######################################################
    ##### Erlang EDoc builder methods and definitions #####
    
    def edocFilesGenerator(source, target, env, for_signature):
        """ Generate the command line to generate the code. """
        
        options = []

        if env.has_key("DIR"):
            options.append(("dir", strInStr(env["DIR"])))
        else:
            # Automatic generation of dir based on the target.
            options.append(("dir", strInStr("doc/")))

        if env.has_key("DEF"):
            defs =  map(lambda (key, value): pyTupleToErlTulpe((key, strInStr(value))), env["DEF"])
            options.append(("def", "[" + ",".join(defs) + "]"))
        
        options = "[" + ",".join(map(pyTupleToErlTulpe, options)) + "]"

        command = "erl -noshell -run edoc_run files '[%s]' '%s'" % (",".join(['"' + str(x) + '"' for x in source]), options)
        
        return command
    
    def documentTargets(target, source, env):
        """ Artifitially create all targets that generating documentation will generate to clean them up latter. """

        # Find the output dir.
        if env.has_key("DIR"):
            dir = env["DIR"]
        else:
            dir = "doc/"
        
        # TODO: What happens if two different sources has the same name on different directories ?
        target = [dir + os.path.splitext(os.path.basename(filename))[0] + ".html"
                  for filename in map(str, source)]
        
        target += [dir + filename for filename in
                   ["edoc-info", "index.html", "modules-frame.html", "overview-summary.html", "stylesheet.css", "packages-frame.html"]]
        
        #newSources = source + [tdir + "overview.edoc"]
        return (target, source)
    
    def edocScanner(node, env, path):
        overview = os.path.dirname(str(node)) + "/overview.edoc"
        if os.path.exists(overview):
            return ["overview.edoc"]
        else:
            return []
    
    edocFilesBuilder = Builder(generator = edocFilesGenerator,
                               emitter = documentTargets,
                               target_scanner = Scanner(function=edocScanner))
    env.Append(BUILDERS = {"EDocFiles" : edocFilesBuilder})

    ##########################
    ##### Helper Methods #####
    
    def outputDir(source, env):
        """ Given a source and its environment, return the output directory.
            The OUTPUTDIR environment variable will be checked first, if it is set to False this function returns False (the user wants us out of the way), otherwise the directory of the OUTPUTDIR will be used.
            If the variable is not set the output directory will be calculate from the source of the file, which is just the directory where the source is unless it ends in src/ in which case the ebin/ counterpart would be used. """

        if env.has_key("OUTPUTDIR"):
            if env["OUTPUTDIR"]:
                if env["OUTPUTDIR"][-1] != "/":
                    return env["OUTPUTDIR"] + "/"
                else:
                    return env["OUTPUTDIR"]
            else:
                False
        else:
            output = dirOf(source)
            if output[-4:] == "src/":
                return output[:-4] + "ebin/"
            else:
                return output
    
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

    def strInStr(str):
        """ Put a string inside a string, that is, the string will contain leading and trailing double quotes so it is a string containing an Erlang string (otherwise it would be a string containing possible an atom, variable name or something just invalid). """
        return '"%s"' % str

    def pyTupleToErlTulpe(tpl):
        """ Turn a Python tuple into a string containing a valid Erlang tuple. """
        return "{" + ",".join(map(str, tpl)) + "}"
    
def exists(env):
    return env.Detect(["erlc"])
