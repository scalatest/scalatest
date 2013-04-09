/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import scala.reflect.BeanProperty

trait BookPropertyMatchers {

  case class Book(
    var title: String,
    val author: String,
    val pubYear: Int,
    val length: Int,
    val isGoodRead: Boolean
  )

/*
  case class JavaBook(
    @BeanProperty var title: String,
    private val author: String,
    @BeanProperty val pubYear: Int,
    private var length: Int,
    private val goodRead: Boolean
  ) {
    def getAuthor: String = author
    def getLength: Int = length
    def setLength(len: Int) { length = len }
    def isGoodRead: Boolean = goodRead
  }
*/

/*

The  BookPropertiesMatcher didn't compile in 2.8, and rightly so. There was a type error in it that wasn't caught by the 2.7 compiler.
Since I'd already decided I didn't like the nested syntax because it isn't readable enough, I am not too concerned this doesn't compile.
Probably better that it doesn't.

  case class Bookshelf(
    val book1: Book,
    val book2: Book,
    val book3: Book
  )

  class BookPropertiesMatcher(firstPropertyMatcher: HavePropertyMatcher[Book, _], propertyMatchers: HavePropertyMatcher[Book, _]*)
      extends HavePropertyMatcher[Bookshelf, Book] {

    def apply(bookshelf: Bookshelf) = {
      val propertyMatcherList = firstPropertyMatcher :: propertyMatchers.toList
      val propertyMatchResults = // This is the list of results
        for (propertyMatcher <- propertyMatcherList) yield
          propertyMatcher(bookshelf.book1)

      val firstFailure = propertyMatchResults.find(_.matches == false)
      firstFailure match {
        case Some(failure) =>
          new HavePropertyMatchResult(false, "book1." + failure.propertyName, failure.expectedValue, failure.actualValue)
        case None =>
          new HavePropertyMatchResult(true, "book1", null, null) // What to do here?
      }
    }
  }

  def book1(firstPropertyMatcher: HavePropertyMatcher[Book, _], propertyMatchers: HavePropertyMatcher[Book, _]*) =
    new BookPropertiesMatcher(firstPropertyMatcher, propertyMatchers: _*)
*/

  class TitleMatcher(expectedValue: String) extends HavePropertyMatcher[Book, String] {
    def apply(book: Book) = {
      new HavePropertyMatchResult(book.title == expectedValue, "title", expectedValue, book.title)
    }
  }

  def title(expectedValue: String) = new TitleMatcher(expectedValue)

  class AuthorMatcher(expectedValue: String) extends HavePropertyMatcher[Book, String] {
    def apply(book: Book) = {
      new HavePropertyMatchResult(book.author == expectedValue, "author", expectedValue, book.author)
    }
  }

  def author(expectedValue: String) = new AuthorMatcher(expectedValue)

  class PubYearMatcher(expectedValue: Int) extends HavePropertyMatcher[Book, Int] {
    def apply(book: Book) = {
      new HavePropertyMatchResult(book.pubYear == expectedValue, "pubYear", expectedValue, book.pubYear)
    }
  }

  def pubYear(expectedValue: Int) = new PubYearMatcher(expectedValue)

  class GoodReadMatcher(expectedValue: Boolean) extends HavePropertyMatcher[Book, Boolean] {
    def apply(book: Book) = {
      new HavePropertyMatchResult(book.isGoodRead == expectedValue, "goodRead", expectedValue, book.isGoodRead)
    }
  }

  class GoodReadBePropertyMatcher extends BePropertyMatcher[Book] {
    def apply(book: Book) = {
      new BePropertyMatchResult(book.isGoodRead, "goodRead")
    }
  }

  def goodRead(expectedValue: Boolean) = new GoodReadMatcher(expectedValue)
  def goodRead = new GoodReadBePropertyMatcher
}

