import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenArrayHelper {

  private def writeFile(targetFile: File, content: String, scalaVersion: String): File = {
    targetFile.getParentFile.mkdirs()
    val destWriter = new BufferedWriter(new FileWriter(targetFile))
    try {
      destWriter.write(content)
      targetFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Generated " + targetFile.getAbsolutePath)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val content =
      if (scalaVersion startsWith "2.13")
        """/*
          | * Copyright 2001-2018 Artima, Inc.
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
          |package org.scalactic
          |
          |private[org] object ArrayHelper {
          |
          |  // The following code is taken from: https://github.com/scala/scala/blob/86e75db7f36bcafdd75302f2c2cca0c68413214d/src/partest/scala/tools/partest/Util.scala
          |  def prettyArray(a: Array[_]): collection.IndexedSeq[Any] = new collection.AbstractSeq[Any] with collection.IndexedSeq[Any] {
          |    def length = a.length
          |
          |    def apply(idx: Int): Any = a(idx) match {
          |      case x: AnyRef if x.getClass.isArray => prettyArray(x.asInstanceOf[Array[_]])
          |      case x => x
          |    }
          |
          |    override def className = "Array"
          |  }
          |
          |  def deep[T](a: Array[T]): collection.IndexedSeq[Any] = prettyArray(a)
          |
          |  def isArrayOps(obj: Any): Boolean = obj.isInstanceOf[scala.collection.ArrayOps[_]]
          |
          |  def asArrayOps(obj: Any): scala.collection.ArrayOps[_] = obj.asInstanceOf[scala.collection.ArrayOps[_]]
          |
          |  def arrayOpsOfInt(a: Array[Int]): scala.collection.ArrayOps[Int] = new scala.collection.ArrayOps(a)
          |}
        """.stripMargin
      else
        """/*
          | * Copyright 2001-2018 Artima, Inc.
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
          |package org.scalactic
          |
          |private[org] object ArrayHelper {
          |
          |  def deep[T](a: Array[T]): collection.IndexedSeq[Any] = a.deep
          |
          |  def isArrayOps(obj: Any): Boolean = obj.isInstanceOf[scala.collection.mutable.ArrayOps[_]]
          |
          |  def asArrayOps(obj: Any): scala.collection.mutable.ArrayOps[_] = obj.asInstanceOf[scala.collection.mutable.ArrayOps[_]]
          |
          |  def arrayOpsOfInt(a: Array[Int]): scala.collection.mutable.ArrayOps[Int] = new scala.collection.mutable.ArrayOps.ofInt(a)
          |
          |}
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"ArrayHelper.scala"), content, scalaVersion)
    )
  }

}