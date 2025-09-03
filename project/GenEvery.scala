import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenEvery {

  def transformLine(line: String, scalaVersion: String): String = {
    if(ScalaVersionHelper.isStdLibCompat_213(scalaVersion)) {
      val scala213Compatible = 
        line
          .replaceAllLiterally(": Map[K, Every[T]]", ": scala.collection.MapView[K, org.scalactic.Every[T]]")
          .replaceAllLiterally("final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = underlying.to[Col](cbf)",
            "final def to[C1](factory: scala.collection.Factory[T, C1]): C1 = underlying.to(factory)")
          .replaceAllLiterally("final def toTraversable: Traversable[T] = underlying.toTraversable", "")
          .replaceAllLiterally("final def union[U >: T](that: GenSeq[U])(implicit cbf: CanBuildFrom[Vector[T], U, Vector[U]]): Every[U] = fromNonEmptyVector(underlying.union(that)(cbf))",
            """final def union[U >: T](that: Seq[U]): Every[U] = {
              |  val vec = underlying.union(that)
              |  Every(vec.head, vec.tail: _*)
              |}
            """.stripMargin)
      if (scalaVersion.startsWith("3.")) 
        scala213Compatible.replace(": _*", "*")
                          .replace("uncheckedVariance => uV", "uncheckedVariance as uV")
      else
        scala213Compatible      
    }
    else
      line 
  }

  private def copyFile(sourceFile: File, destFile: File, scalaVersion: String): File = {
    destFile.getParentFile.mkdirs()
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(transformLine(line, scalaVersion))
        destWriter.newLine()
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Generated " + destFile.getAbsolutePath)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    Seq(
      copyFile(new File("project/templates/Every.template"), new File(targetDir, "Every.scala"), scalaVersion)
    )

}