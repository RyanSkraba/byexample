package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import java.nio.file.Path

/** Trait for including file system helpers. */
trait FileRenamerSpecBase { this: MultiTaskMainSpec[_] with TmpDir =>

  /** Creates a scenario in the temporary directory with some files and directories in it
    *
    * @param tag
    *   A string tag to use to uniquely identify the scenario
    * @param srcPaths
    *   Files and directories relative to the source directory to create. Directories end with a slash, files are filled
    *   with the text of their filename.
    * @return
    *   A source directory potentially populated with files, and an empty destination directory.
    */
  def createSrcDst(tag: String, srcPaths: String*): (Path, Path) = {
    val src = (Tmp / tag / "src").createDirectory(failIfExists = false)
    val dst = (Tmp / tag / "dst").createDirectory(failIfExists = false)
    for (p <- srcPaths) {
      val srcPath = src / p
      if (p.endsWith("/"))
        srcPath.createDirectory(failIfExists = false)
      else {
        srcPath.getParent.createDirectory(failIfExists = false)
        srcPath.createFile().writeAll(srcPath.name)
      }
    }
    (src, dst)
  }

  /** A helper method for running an ammonite script with an assumed successful result.
    * @param replacements
    *   A list of pairs of strings to replace in the output.
    * @param task
    *   The task to run
    * @param args
    *   The arguments to apply to the ammonite script
    * @return
    *   The output of the script with all of the string replacements applied, as well as replacing the temporary
    *   directory with &lt;TMP&gt;.
    */
  def withTaskSuccess(replacements: (String, String)*)(args: Any*): String = {
    withGoMatching(args: _*) { case (stdout: String, stderr: String) =>
      stderr shouldBe empty
      replacements
        .foldLeft(stdout) { (acc, r) => acc.replace(r._1, r._2) }
        .replace(Tmp.toString, "<TMP>")
    }
  }

  /** A helper method for running an ammonite script with an assumed successful result, specifically for scenarios
    * involving source and destination directories.
    *
    * @param src
    *   The source directory.
    * @param dst
    *   The destination directory.
    * @param replacements
    *   A list of pairs of strings to replace in the output.
    * @param task
    *   The task to run.
    * @param args
    *   The arguments to apply to the ammonite script.
    * @return
    *   The output of the script with all of the string replacements applied, as well as replacing the source and
    *   destination directories with &lt;SRC&gt; and &lt;DST&gt; respectively.
    */
  def withTaskSuccessSrcDst(src: Path, dst: Path, replacements: (String, String)*)(args: Any*): String =
    withTaskSuccess(replacements ++ Seq(src.toString -> "<SRC>", dst.toString -> "<DST>"): _*)(args: _*)

}
