//
// Converts calls to deprecated 'feature' to 'Feature' and
// 'scenario' to 'Scenario'.
//
// If a directory is specified as an argument, processes any
// .scala files it finds under that directory tree.
//
// Renames original files as <filename>.feature.
//
import java.io.File
import java.io.PrintWriter
import java.util.regex.Pattern
import scala.io.Source

object Main {

  def usage(): Unit = {
    println("Usage: scala feature2Feature.scala FILE|DIRECTORY...")
    System.exit(-1)
  }

  def main(args: Array[String]) {
    if (args.length == 0)
      usage()

    processFiles(args)
  }

  def processFiles(files: Seq[String]): Unit = {
    files.map(new File(_)).foreach { file =>
      if (!file.exists)
        System.err.println(s"not found: $file")
      else if (file.isDirectory)
        processFiles(findScalaFiles(file))
      else
        processFile(file)
    }
  }

  def findScalaFiles(dir: File): List[String] = {
    findScalaFiles(dir.listFiles)
  }
    
  def findScalaFiles(fileList: Seq[File]): List[String] = {
    if (fileList.isEmpty) Nil
    else {
      val file = fileList.head

      if (file.isDirectory)
        findScalaFiles(file) ::: findScalaFiles(fileList.tail)
      else if (file.getName.endsWith(".scala"))
        file.getPath :: findScalaFiles(fileList.tail)
      else
        findScalaFiles(fileList.tail)
    }
  }

  def readFile(file: File): String = Source.fromFile(file).mkString

  def writeFile(filespec: String, text: String) {
    val out = new PrintWriter(filespec)
    out.print(text)
    out.close()
  }

  def processFile(file: File): Unit = {
    val text = readFile(file)

    val replTextF = text.replaceAll("""\bfeature(\s*\(\s*")""", "Feature$1")

    val replText =
      replTextF.replaceAll("""\bscenario(\s*\(\s*")""", "Scenario$1")

    if (text != replText) {
      val originalFile: String = file.getPath
      println(s" modified $originalFile")
      file.renameTo(new File(originalFile + ".feature"))
      writeFile(originalFile, replText)
    }
  }
}

Main.main(args)
