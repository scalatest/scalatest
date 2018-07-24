import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenChain {

  def transformLine(line: String, scalaVersion: String): String = {
    if (scalaVersion startsWith "2.13")
      line
        .replaceAllLiterally(": Map[K, Chain[T]]", ": scala.collection.MapView[K, Chain[T]]")
        .replaceAllLiterally("final def toTraversable: Traversable[T] = toList.toTraversable", "")
        .replaceAllLiterally("final def union[U >: T](that: GenSeq[U])(implicit cbf: CanBuildFrom[List[T], U, List[U]]): Chain[U] = new Chain(toList.union(that)(cbf))",
                             """final def union[U >: T](that: Seq[U]): Chain[U] = {
                               |  val l = toList.union(that)
                               |  Chain(l.head, l.tail: _ *)
                               |}
                             """.stripMargin)
        .replaceAllLiterally("def ++[U >: T](other: GenTraversableOnce[U]): Chain[U]", "def ++[U >: T](other: scala.collection.IterableOnce[U]): Chain[U]")
        .replaceAllLiterally("final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = toList.to[Col](cbf)",
                             "final def to[C1](factory: scala.collection.Factory[T, C1]): C1 = toList.to(factory)")
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
      copyFile(new File("project/templates/Chain.scala.template"), new File(targetDir, "Chain.scala"), scalaVersion)
    )

}