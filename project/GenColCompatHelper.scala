import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenColCompatHelper {

  private def writeFile(targetFile: File, content: String): File = {
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
          |  def aggregate[A, B](col: Iterable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.foldLeft(z)(seqop)
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
          |  def aggregate[A, B](col: scala.collection.GenTraversable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.aggregate(z)(seqop, combop)
          |
          |}
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"ColCompatHelper.scala"), content)
    )
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val chainSpec =
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
          |import scala.collection.mutable.ListBuffer
          |
          |import org.scalatest._
          |
          |class GeneratedChainSpec extends FlatSpec with Matchers with OptionValues with Inside with TypeCheckedTripleEquals {
          |
          |  "A Chain" should "have a to method" in {
          |    Chain(1).to(List) shouldBe List(1)
          |    Chain(1, 2, 3).to(List) shouldBe List(1, 2, 3)
          |    Chain(1, 2, 3).to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(1, 2, 3)
          |    Chain(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
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
          |package org.scalactic
          |
          |import scala.collection.mutable.ListBuffer
          |
          |import org.scalatest._
          |
          |class GeneratedChainSpec extends FlatSpec with Matchers with OptionValues with Inside with TypeCheckedTripleEquals {
          |
          |  "A Chain" should "have a to method" in {
          |    Chain(1).to[List] shouldBe List(1)
          |    Chain(1, 2, 3).to[List] shouldBe List(1, 2, 3)
          |    Chain(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(1, 2, 3)
          |    Chain(1, 2, 3).to[Vector] shouldBe Vector(1, 2, 3)
          |  }
          |
          |  it should "have a toTraversable method" in {
          |    Chain(1, 2, 3).toTraversable should === (Traversable(1, 2, 3))
          |    Chain("a", "b").toTraversable should === (Traversable("a", "b"))
          |    Chain(1).toTraversable should === (Traversable(1))
          |  }
          |
          |}
        """.stripMargin

    val everySpec =
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
          |import scala.collection.mutable.ListBuffer
          |
          |import org.scalatest._
          |
          |class GeneratedEverySpec extends FlatSpec with Matchers with OptionValues with Inside with TypeCheckedTripleEquals {
          |
          |  "An Every" should "have a to method" in {
          |    Every(1).to(List) shouldBe List(1)
          |    Every(1, 2, 3).to(List) shouldBe List(1, 2, 3)
          |    Every(1, 2, 3).to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(1, 2, 3)
          |    Every(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
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
          |package org.scalactic
          |
          |import scala.collection.mutable.ListBuffer
          |
          |import org.scalatest._
          |
          |class GeneratedEverySpec extends FlatSpec with Matchers with OptionValues with Inside with TypeCheckedTripleEquals {
          |
          |  "An Every" should "have a to method" in {
          |    Every(1).to[List] shouldBe List(1)
          |    Every(1, 2, 3).to[List] shouldBe List(1, 2, 3)
          |    Every(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(1, 2, 3)
          |    Every(1, 2, 3).to[Vector] shouldBe Vector(1, 2, 3)
          |  }
          |
          |  it should "have a toTraversable method" in {
          |    Every(1, 2, 3).toTraversable should === (Traversable(1, 2, 3))
          |    Many("a", "b").toTraversable should === (Traversable("a", "b"))
          |    One(1).toTraversable should === (Traversable(1))
          |  }
          |
          |}
        """.stripMargin

    Seq(
      writeFile(new File(targetDir,"GeneratedChainSpec.scala"), chainSpec),
      writeFile(new File(targetDir,"GeneratedEverySpec.scala"), everySpec)
    )
  }

}