import java.io.{FileWriter, BufferedWriter, File}

object GenCompatibleClasses {

  val generatorSource = new File("GenCompatibleClasses.scala")

  def genMain(targetDir: File, version: String, scalaVersion: String, isJvm: Boolean): Seq[File] = {
    targetDir.mkdirs()
    val listCellRendererClass = Class.forName("javax.swing.ListCellRenderer")
    val isJava7 = listCellRendererClass.getTypeParameters.length > 0

    val java6Classes =
      Map(
        "EventHolderJList" -> "private[tools] class EventHolderJList extends JList",
        "EventHolderDefaultListModel" -> "private[tools] class EventHolderDefaultListModel extends DefaultListModel"
      )

    val java6ClassesFiles =
      java6Classes.map { case (name, cls) =>
        val file = new File(targetDir, name + ".scala")
        if (!file.exists || generatorSource.lastModified > file.lastModified) {
          val bw = new BufferedWriter(new FileWriter(file))
          try {
            bw.write("package org.scalatest.tools\n")
            bw.write("import javax.swing._\n")
            if (isJava7)
              bw.write(cls + "[EventHolder]")
            else
              bw.write(cls)
          }
          finally {
            bw.flush()
            bw.close()
          }
          println("Generated " + file.getAbsolutePath)
        }
        file
      }

    val file = new File(targetDir, "EventHolderListCellRenderer.scala")
    if (!file.exists || generatorSource.lastModified > file.lastModified) {
      val bw = new BufferedWriter(new FileWriter(file))

      try {
        bw.write("package org.scalatest.tools\n")
        bw.write("import java.awt.Component\n")
        bw.write("import javax.swing._\n")
        if (isJava7) { // workaround from http://www.scala-lang.org/old/node/10687
          bw.write("private[tools] trait EventHolderListCellRenderer extends ListCellRenderer[EventHolder] {\n")
          bw.write("  private val defaultRenderer: ListCellRenderer[EventHolder] = (new DefaultListCellRenderer()).asInstanceOf[ListCellRenderer[EventHolder]]\n")
          bw.write("  protected def decorate(renderer: JLabel, value: Object, isSelected: Boolean): Component\n")
          bw.write("  def getListCellRendererComponent(list: JList[_ <: EventHolder], value: EventHolder, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {\n")
          bw.write("    val renderer: JLabel = defaultRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus).asInstanceOf[JLabel]\n")
          bw.write("    decorate(renderer, value, isSelected)\n")
          bw.write("  }")
          bw.write("}\n")
        }
        else {
          bw.write("private[tools] trait EventHolderListCellRenderer extends ListCellRenderer {\n")
          bw.write("  private val defaultRenderer: DefaultListCellRenderer = new DefaultListCellRenderer()\n")
          bw.write("  protected def decorate(renderer: JLabel, value: Object, isSelected: Boolean): Component\n")
          bw.write("  def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {\n")
          bw.write("    val renderer: JLabel = defaultRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus).asInstanceOf[JLabel]\n")
          bw.write("    decorate(renderer, value, isSelected)\n")
          bw.write("  }")
          bw.write("}\n")
        }
      }
      finally {
        bw.flush()
        bw.close()
      }
    }   

    if (isJvm)
      java6ClassesFiles.toSeq ++ Seq(file)
    else
      Seq.empty
  }

  def genTest(baseTargetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val targetDir = new File(baseTargetDir, "scala/org/scalatest")
    targetDir.mkdirs()
    val file = new File(targetDir, "CompatParColls.scala")
    if (!file.exists || generatorSource.lastModified > file.lastModified) {
      val parMethod = if(ScalaVersionHelper.isStdLibCompat_213(scalaVersion)) "def par: T = oriCol" else ""
      /*
       For recording purpose, this is the original version of CompatParColls that stops working in 2.13.0-M4
        /**
        * This compatibility workaround is taken from https://github.com/scala/scala-parallel-collections/issues/22
        */
private[org] object CompatParColls {
 val Converters = {
   import Compat._
    {
     import scala.collection.parallel._
      CollectionConverters
   }
 }
  object Compat {
   object CollectionConverters
  }
}
      */
      val content =
        """package org.scalatest
          |
          |private[org] object CompatParColls {
          |
          |  object Converters {
          |    class MockParallelCol[T](oriCol: T) {
          |      $$PAR_METHOD$$
          |    }
          |    implicit def convertToParallel[T](col: T): MockParallelCol[T] = new MockParallelCol(col)
          |  }
          |}
          |
        """.stripMargin.replaceAllLiterally("$$PAR_METHOD$$", parMethod)
      val bw = new BufferedWriter(new FileWriter(file))
      try {
        bw.write(content)
      }
      finally {
        bw.flush()
        bw.close()
      }
    }
    Seq(file)
  }

  def main(args: Array[String]) {

    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)

    val mainDir = new File(targetDir + "/main/scala/org/scalatest/tools")
    genMain(mainDir, version, scalaVersion, true) // Always true for isJvm, because ant build does not support Scala.js and native.
  }

}