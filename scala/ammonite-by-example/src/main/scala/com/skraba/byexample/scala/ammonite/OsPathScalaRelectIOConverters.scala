package com.skraba.byexample.scala.ammonite

import scala.language.implicitConversions

object OsPathScalaRelectIOConverters {
  implicit def reflectPathToOsPath(p: scala.reflect.io.Path): os.Path = os.Path(p.jfile)
  implicit def reflectFileToOsPath(f: scala.reflect.io.File): os.Path = os.Path(f.jfile)
  implicit def reflectDirectoryToOsPath(d: scala.reflect.io.Directory): os.Path = os.Path(d.jfile)
  implicit def osPathToReflectPath(p: os.Path): scala.reflect.io.Path = scala.reflect.io.Path(p.toIO)
  implicit def osPathToReflectFile(p: os.Path): scala.reflect.io.File = scala.reflect.io.File(p.toIO)
  implicit def osPathToReflectDirectory(p: os.Path): scala.reflect.io.Directory = scala.reflect.io.Directory(p.toIO)
}
