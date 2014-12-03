MMusicScala
===========
With Scala 2.11

To use :

Take the project from https://github.com/vtpittet/ScalaMusicGeneration
In ScalaMusicGeneration/MusicInterface, do sbt eclipse
Import the project into eclipse
Import this project into eclipse

Then :
Clone the app branch of https://github.com/regb/scabolic
Revert it to the commit a1c6d3045ac880d33da6f1ecfbafb1f4c4f0aa2c

Clone near to it the project https://github.com/regb/scala-smtlib
Revert it to the commit 004fab30fc294677a14429fad2cd95ab4d366416
(git reset --hard 004fab30fc294677a14429fad2cd95ab4d366416)

In file scabolic/project/Build.scala, comment line 29 (lazy val depProject = RootProject(uri("git://github.com/regb/scala-smtlib.git#%s".format(V.depProject))))
and replace it by lazy val depProject = RootProject( file("<relative path to scala-smtlib folder>") ), without <>

do sbt package in folder scabolic, find the jar in scabolic/target/scala-2.11

configure build path of MMusicScala project to it
=======
COMPILING AND RUNNING //this part is not up to date



The music interface code can be compiled from the MusicInterface directory.

First make sure to have a fresh bin directory.

```bash
rm -rf bin; mkdir bin
```

Then, compile using the command

```bash

fsc -d bin/ \
    src/midiInterface/*.scala \
    src/rythmics/*.scala \
    src/segmentSystem/*.scala \
    src/tonalSystem/*.scala \
    src/utils/*.scala \
    test/caseStudy/*.scala
```
Finally you can run, for example the Recuerdos case study, executing
```bash
scala -cp bin/ caseStudy.Recuerdos
```

## Workflow using SBT

* `sbt run`
* `sbt publish-local` to use from other projects
* in order project, add `libraryDependencies += "epfl" % "irgen_2.11" % "1.0-SNAPSHOT"` to `build.sbt`
