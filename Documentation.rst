SCons Erlang 0.1.99
===================

SCons Erlang provides two builders, one for building Erlang applications (program or libraries, both are know as applications in Erlang jargon) and one for building the API documentation of an Erlang application using EDoc.

Erlang applications
-------------------

The builder for the Erlang applications is called 'Erlang' and takes the following parameters:

- sources: a list of the sources to be built.
- PATHPREPEND (optional): A path to prepend to the path to search for libraries, passed to the -pa argument.
- PATHAPEND (optional): Same as before but appending, that is, using -pz.
- ERLFLAG (optional): Extra flags to pass to erlc, the compiler.
