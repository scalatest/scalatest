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
      if (ScalaVersionHelper.isStdLibCompat_213(scalaVersion))
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
          |import scala.annotation.unchecked.{ uncheckedVariance => uV }
          |
          |private[org] object ColCompatHelper {
          |
          |  type IndexedSeqLike[+A, +Repr] = scala.collection.IndexedSeqOps[A, IndexedSeq, Repr]
          |
          |  def aggregate[A, B](col: Iterable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.foldLeft(z)(seqop)
          |
          |  type WithFilter[+A, +CC[_]] = scala.collection.WithFilter[A, CC]
          |
          |  type IterableOnce[+A] = scala.collection.IterableOnce[A]
          |
          |  type Factory[-A, +C] = scala.collection.Factory[A, C]
          |
          |  object Factory {}
          |
          |  def className(col: scala.collection.Iterable[_]): String = {
          |    val colToString = col.toString
          |    val bracketIdx = colToString.indexOf("(")
          |    if (bracketIdx >= 0)
          |      colToString.take(bracketIdx)
          |    else
          |      org.scalactic.NameUtil.getSimpleNameOfAnObjectsClass(col)
          |  }
          |
          |  def newBuilder[A, C](f: Factory[A, C]): scala.collection.mutable.Builder[A, C] = f.newBuilder
          |
          |  type StringOps = scala.collection.StringOps
          |
          |  class InsertionOrderSet[A](elements: List[A]) extends scala.collection.immutable.Set[A] {
          |    private val underlying = scala.collection.mutable.LinkedHashSet(elements: _*)
          |    def contains(elem: A): Boolean = underlying.contains(elem)
          |    def iterator: Iterator[A] = underlying.iterator
          |    def excl(elem: A): scala.collection.immutable.Set[A] = new InsertionOrderSet(elements.filter(_ != elem))
          |    def incl(elem: A): scala.collection.immutable.Set[A] = 
          |      if (underlying.contains(elem)) 
          |        new InsertionOrderSet(elements) 
          |      else 
          |        new InsertionOrderSet(if (underlying.contains(elem)) elements else elements :+ elem)
          |  }
          |}
          |
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
          |import scala.annotation.unchecked.{ uncheckedVariance => uV }
          |
          |private[org] object ColCompatHelper {
          |
          |  type IndexedSeqLike[+A, +Repr] = scala.collection.IndexedSeqLike[A, Repr]
          |
          |  def aggregate[A, B](col: scala.collection.GenTraversable[A], z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = col.aggregate(z)(seqop, combop)
          |
          |  type WithFilter[+A, +Repr] = scala.collection.generic.FilterMonadic[A, Repr]
          |
          |  type IterableOnce[+A] = scala.collection.GenTraversableOnce[A]
          |
          |  type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C] // Ideally, this would be an opaque type
          |
          |  object Factory {
          |
          |    def simpleCBF[A, C](f: => scala.collection.mutable.Builder[A, C]): scala.collection.generic.CanBuildFrom[Any, A, C] =
          |      new scala.collection.generic.CanBuildFrom[Any, A, C] {
          |        def apply(from: Any): scala.collection.mutable.Builder[A, C] = apply()
          |        def apply(): scala.collection.mutable.Builder[A, C]          = f
          |      }
          |
          |    implicit def fromCanBuildFrom[A, C](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, A, C]): Factory[A, C] =
          |      cbf.asInstanceOf[Factory[A, C]]
          |
          |    implicit def fromCanBuildFromConversion[X, A, C](x: X)(
          |      implicit toCanBuildFrom: X => scala.collection.generic.CanBuildFrom[Nothing, A, C]): Factory[A, C] =
          |      fromCanBuildFrom(toCanBuildFrom(x))
          |
          |    implicit def genericCompanionToCBF[A, CC[X] <: scala.collection.GenTraversable[X]](
          |      fact: scala.collection.generic.GenericCompanion[CC]): scala.collection.generic.CanBuildFrom[Any, A, CC[A]] =
          |      simpleCBF(fact.newBuilder[A])
          |
          |    implicit def arrayCompanionToCBF[A: scala.reflect.ClassTag](fact: Array.type): scala.collection.generic.CanBuildFrom[Any, A, Array[A]] =
          |      simpleCBF(Array.newBuilder[A])
          |  }
          |  def className(col: scala.collection.GenTraversable[_]): String = col.stringPrefix
          |
          |  def newBuilder[A, C](f: Factory[A, C]): scala.collection.mutable.Builder[A, C] = f.apply()
          |
          |  type StringOps = scala.collection.immutable.StringOps
          |
          |  class InsertionOrderSet[A](elements: List[A]) extends scala.collection.immutable.Set[A] {
          |    private val underlying = scala.collection.mutable.LinkedHashSet(elements: _*)
          |    def contains(elem: A): Boolean = underlying.contains(elem)
          |    def iterator: Iterator[A] = underlying.iterator
          |    def -(elem: A): scala.collection.immutable.Set[A] = new InsertionOrderSet(elements.filter(_ != elem))
          |    def +(elem: A): scala.collection.immutable.Set[A] = 
          |      if (underlying.contains(elem)) 
          |        new InsertionOrderSet(elements) 
          |      else 
          |        new InsertionOrderSet(if (underlying.contains(elem)) elements else elements :+ elem)
          |  }
          |}
          |
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"ColCompatHelper.scala"), content)
    )
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val chainSpec =
      if (ScalaVersionHelper.isStdLibCompat_213(scalaVersion))
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
      if (ScalaVersionHelper.isStdLibCompat_213(scalaVersion))
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