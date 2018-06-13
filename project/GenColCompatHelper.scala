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
          |}
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"ArrayHelper.scala"), content, scalaVersion)
    )
  }

}