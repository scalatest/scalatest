import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenColCompatHelper {

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
          |package org.scalatest
          |
          |private[scalatest] object ColCompatHelper {
          |
          |  type IndexedSeqLike[+A, +Repr] = scala.collection.IndexedSeqOps[A, IndexedSeq, Repr]
          |
          |  type MapLike[K, +V, +This <: scala.collection.MapOps[K, V, Map, This] with scala.collection.Map[K, V]] = scala.collection.MapOps[K, V, Map, This]
          |
          |  def aggregate[A, B](col: Iterable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.foldLeft(z)(seqop)
          |
          |  trait CompatConfigMap extends Map[String, Any] {
          |    def toMap: Map[String, Any]
          |    def remove(key: String): ConfigMap = new ConfigMap(toMap.remove(key))
          |    def updated[V1 >: Any](key: String, value: V1): ConfigMap = new ConfigMap(toMap.updated(key, value))
          |  }
          |
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
          |package org.scalatest
          |
          |private[scalatest] object ColCompatHelper {
          |
          |  type IndexedSeqLike[+A, +Repr] = scala.collection.IndexedSeqLike[A, Repr]
          |
          |  type MapLike[K, +V, +This <: scala.collection.MapLike[K, V, This] with scala.collection.Map[K, V]] = scala.collection.MapLike[K, V, This]
          |
          |  def aggregate[A, B](col: scala.collection.GenTraversable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.aggregate(z)(seqop, combop)
          |
          |  trait CompatConfigMap extends Map[String, Any] {
          |    def toMap: Map[String, Any]
          |    def -(key: String): ConfigMap =  new ConfigMap(toMap.filter(_._1 != key))
          |  }
          |}
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"ColCompatHelper.scala"), content, scalaVersion)
    )
  }

}