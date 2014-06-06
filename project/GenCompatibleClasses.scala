import java.io.{FileWriter, BufferedWriter, File}

object GenCompatibleClasses {

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    targetDir.mkdirs()
    val listCellRendererClass = Class.forName("javax.swing.ListCellRenderer")
    val isJava7 = listCellRendererClass.getTypeParameters.length > 0

    val java6Classes =
      Map(
        "EventHolderJList" -> "private[tools] class EventHolderJList extends JList",
        "EventHolderDefaultListModel" -> "private[tools] class EventHolderDefaultListModel extends DefaultListModel"
      )

    java6Classes.foreach { case (name, cls) =>
      val file = new File(targetDir, name + ".scala")
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

    val file = new File(targetDir, "EventHolderListCellRenderer.scala")
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

  def main(args: Array[String]) {

    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)

    val mainDir = new File(targetDir + "/main/scala/org/scalatest/tools")
    genMain(mainDir, version, scalaVersion)
  }

}