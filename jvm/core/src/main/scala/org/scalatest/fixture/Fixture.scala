/*
 * Copyright 2001-2024 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalatest

/**
  * <code>Fixture[A]</code> is a trait for composable fixtures. The purpose
  * is to enable the creation of larger, more complex fixtures from simpler
  * ones. They are primarily intended to be used in conjunction with the
  * <code>GenericFixtureSuite</code> trait; see that trait's documentation
  * for details.
  * 
  * The core of the <code>Fixture</code> type is its abstract
  * <code>apply</code> method. Although you can create a fixture by 
  * extending the trait and implementing this method, it is usually more
  * convenient to use one of the factory methods in the companion object:
  * <code>autoCloseable</code>, <code>beforeAndAfter</code> and
  * <code>testData</code>
  * 
  * What makes the <code>Fixture</code> type composable is the fact that it
  * has <code>map</code> and <code>flatMap</code> methods. This makes it
  * possible to use Scala's <code>for</code> comprehension syntax to build
  * more complex fixtures.
  * For example, you can create a temporary file with the test's name as
  * follows.
  * <code>
  * import java.nio.file.Files
  * for {
  *   testData <- Fixture.testData
  *   path <- Fixture.beforeAndAfter(Files.createTempFile(testData.name, null))(Files.delete)
  * } yield path
  * </code>
  * 
  */

trait Fixture[+A] { self =>
  def apply[X](testData: TestData, f: A => X): X
  
  def map[B](f: A => B): Fixture[B] =
    new Fixture[B] {
      def apply[X](testData: TestData, b: B => X): X =
        self(testData, f.andThen(b))
    }

  def flatMap[B](f: A => Fixture[B]): Fixture[B] =
    new Fixture[B] {
      override def apply[X](testData: TestData, b: B => X): X =
        self(testData, f(_)(testData, b))
    }
}

object Fixture {
  /**
    * Creates a Fixture from a block that allocates an <code>AutoCloseable</code>
    * resource.
    * After the test, the resource will be closed, regardless of how the test
    * ended
    * @param a a block to create the resource
    * @return the fixture
    */
  def autoCloseable[A <: AutoCloseable](a: => A): Fixture[A] =
    beforeAndAfter(a)(_.close())
  
  /**
    * Create a <code>Fixture</code> from two blocks that will run before
    * and after the test.
    * For example, you can create a temporary file and remove it after
    * the test like this:
    * <code>
    * import java.nio.file.Files
    * 
    * def tempFile(prefix: String): Fixture[Path] =
    *   Fixture.beforeAndAfter(
    *     Files.createTempFile(prefix, null)
    *   )(Files.delete)
    * </code>
    * @param before Block that will be executed before the test
    * @param after Block that will be executed after the test.
    *              The result of the <code>before</code> block
    *              will be passed to this block.
    * @return the <code>Fixture</code>
    */
  def beforeAndAfter[A](before: => A)(after: A => Any): Fixture[A] =
    new Fixture[A] {
      def apply[X](testData: TestData, f: A => X): X = {
        val a = before
        try f(a)
        finally after(a)
      }
    }

    /**
      * This fixture gives you access to the <code>TestData</code>
      * object of the current test.
      */
  val testData: Fixture[TestData] =
    new Fixture[TestData] {
      def apply[X](testData: TestData, f: TestData => X): X =
        f(testData)
    }

}
