import java.io.{FileWriter, BufferedWriter, File}

object GenCompatibleClasses {

  val generatorSource = new File("GenCompatibleClasses.scala")

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
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

    val usingManagerClass = 
      if (ScalaVersionHelper.isStdLibCompat_213(scalaVersion)) 
        "scala.util.Using"
      else
        "org.scalactic.Using"

    val resourceManagerFixtureContent = 
      s"""
        |/*
        | * Copyright 2001-2025 Artima, Inc.
        | *
        | * Licensed under the Apache License, Version 2.0 (the "License");
        | * you may not use this file except in compliance with the License.
        | * You may obtain a copy of the License at
        | *
        | *     http://www.apache.org/licenses/LICENSE-2.0
        | *
        | * Unless required by applicable law or agreed to in writing, software
        | * distributed under the License is distributed on an "AS IS" BASIS,
        | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
        | * See the License for the specific language governing permissions and
        | * limitations under the License.
        | */
        |package org.scalatest
        |package fixture
        |
        |import $usingManagerClass
        |
        |/**
        | * A Trait that facilitates the management of resources that need to be
        | * disposed of after using them.
        | * It provides two separate instances of the <code>Using.Manager</code>
        | * class from the Scala standard library (see the Scala standard library
        | * documentationon <code>scala.util.Using</code> for details on how it
        | * works).
        | * 
        | * The first instance can be obtained by calling the <code>suiteScoped</code>
        | * method. Its purpose is to clean up resources that are shared between
        | * different tests in a suite. This method can only be called after test
        | * execution has started. It is therefore recommended to use it to initialize
        | * a <code>lazy val</code> in the Suite class. This way, the 
        | * <code>lazy val</code> can be used to access the resource while deferring
        | * the <code>suiteScoped</code> call until test execution has started.
        | * 
        | * The second instance is passed as a fixture parameter to the test.
        | * It is cleaned up after the test has completed and can therefore be used
        | * to clean up resources that are only required inside a single test.
        | *
        |*/
        |trait ResourceManagerFixture extends FixtureTestSuite {
        |  override type FixtureParam = Using.Manager
        |
        |  private var suiteManagerVar: Using.Manager = null
        |
        |  protected def suiteScoped = Option(suiteManagerVar).getOrElse {
        |    throw new IllegalStateException(
        |      "`suiteScoped`` cannot be called from outside a test. " +
        |      "Use a `lazy val` to store Suite-scoped resources in order to defer " +
        |      "initialization of such resources until the start of a test."
        |    )
        |  }
        |
        |  protected def withFixture(test: OneArgTest): Outcome =
        |    Using.Manager { manager =>
        |      withFixture(test.toNoArgTest(manager))
        |    }.get
        |
        |  override protected def runTests(testName: Option[String], args: Args): Status =
        |    Using.Manager { manager =>
        |      suiteManagerVar = manager
        |      super.runTests(testName, args)
        |    }.get
        |}
      """.stripMargin
    val resourceManagerFixtureFile = new File(targetDir, "ResourceManagerFixture.scala")
    if (!resourceManagerFixtureFile.exists || generatorSource.lastModified > resourceManagerFixtureFile.lastModified) {
      val bw = new BufferedWriter(new FileWriter(resourceManagerFixtureFile))
      try {
        bw.write(resourceManagerFixtureContent)
      }
      finally {
        bw.flush()
        bw.close()
      }
    }   

    java6ClassesFiles.toSeq ++ Seq(file, resourceManagerFixtureFile)
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
    genMain(mainDir, version, scalaVersion)
  }

}