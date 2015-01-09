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