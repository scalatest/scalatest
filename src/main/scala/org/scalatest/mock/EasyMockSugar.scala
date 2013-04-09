/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.mock

import org.scalatest._
import org.easymock.IExpectationSetters
import org.easymock.EasyMock
import org.easymock.EasyMock.{expect => easyMockExpect, expectLastCall}
import scala.reflect.Manifest

/**
 * Trait that provides some basic syntax sugar for <a href="http://easymock.org/" target="_blank">EasyMock</a>.
 *
 * <p>
 * Using the EasyMock API directly, you create a mock with:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = createMock(classOf[Collaborator])
 * </pre>
 *
 * <p>
 * With this trait, you can shorten that to:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = mock[Collaborator]
 * </pre>
 *
 * <p>
 * After creating mocks, you set expectations on them, using syntax like this:
 * </p>
 *
 * <pre class="stHighlight">
 * mockCollaborator.documentAdded("Document")
 * mockCollaborator.documentChanged("Document")
 * expectLastCall().times(3)
 * </pre>
 *
 * <p>
 * If you wish to highlight which statements are setting expectations on the mock (versus
 * which ones are actually using the mock), you can place them in an <code>expecting</code>
 * clause, provided by this trait, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * expecting {
 *   mockCollaborator.documentAdded("Document")
 *   mockCollaborator.documentChanged("Document")
 *   lastCall.times(3)
 * }
 * </pre>
 *
 * <p>
 * Using an <code>expecting</code> clause is optional, because it does nothing but visually indicate
 * which statements are setting expectations on mocks. (Note: this trait also provides the <code>lastCall</code>
 * method, which just calls <code>expectLastCall</code>.)
 * </p>
 *
 * <p>
 * Once you've set expectations on the mock objects, you must invoke <code>replay</code> on
 * the mocks to indicate you are done setting expectations, and will start using the mock.
 * After using the mock, you must invoke <code>verify</code> to check to make sure the mock
 * was used in accordance with the expectations you set on it. Here's how that looks when you
 * use the EasyMock API directly:
 * </p>
 *
 * <pre class="stHighlight">
 * replay(mockCollaborator)
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * verify(mockCollaborator)
 * </pre>
 *
 * <p>
 * This trait enables you to use the following, more declarative syntax instead:
 * </p>
 *
 * <pre class="stHighlight">
 * whenExecuting(mockCollaborator) {
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 * }
 * </pre>
 *
 * <p>
 * The <code>whenExecuting</code> method will pass the <code>mockCollaborator</code> to
 * <code>replay</code>, execute the passed function (your code that uses the mock), and
 * call <code>verify</code>, passing in the <code>mockCollaborator</code>. If you want to
 * use multiple mocks, you can pass multiple mocks to <code>whenExecuting</code>.
 * </p>
 *
 * <p>
 * To summarize, here's what a typical test using <code>EasyMockSugar</code> looks like:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = mock[Collaborator]
 *
 * expecting {
 *   mockCollaborator.documentAdded("Document")
 *   mockCollaborator.documentChanged("Document")
 *   lastCall.times(3)
 * }
 *
 * whenExecuting(mockCollaborator) {
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 * }
 * </pre>
 *
 * <p>
 * An alternative approach is to place your mock objects in a <code>MockObjects</code> holder object referenced
 * from an implicit <code>val</code>, then use the overloaded variant of <code>whenExecuting</code> that
 * takes an implicit <code>MockObjects</code> parameter. Here's how that would look:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val mocks = MockObjects(mock[Collaborator])
 *
 * expecting {
 *   mockCollaborator.documentAdded("Document")
 *   mockCollaborator.documentChanged("Document")
 *   lastCall.times(3)
 * }
 *
 * whenExecuting {
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 * }
 * </pre>
 *
 * <p>
 * Note: As of ScalaTest 1.3, this trait supports EasyMock 3, with no dependencies on EasyMock class extension.
 * </p>
 *
 * @author Bill Venners
 * @author George Berger
 */
trait EasyMockSugar {

  /**
   * Implicit conversion that invokes the <code>expect</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>expect</code> method in Java class <code>org.easymock.EasyMock</code>).
   *
   * <p>
   * In a ScalaTest <code>Suite</code>, the <code>expect</code> method defined in <code>Assertions</code>, and inherited by <code>Suite</code>,
   * interferes with the <code>expect</code> method if imported from <code>EasyMock</code>. You can invoke it by qualifying it, <em>i.e.</em>,
   * <code>EasyMock.expect</code>, or by changing its name on import, like this:
   *
   * <pre class="stHighlight">
   * import org.easymock.EasyMock.{expect => easyMockExpect, _}
   * </pre>
   *
   * <p>
   * But if you mix in this trait, you can just invoke <code>call</code> instead.
   * </p>
   *
   * <p>
   * You can use this method, for example, to chain expectations like this:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting {
   *   call(mock.getName).andReturn("Ben Franklin")
   * }
   * </pre>
   *
   * <p>
   * Note: the name of this methods is <code>call</code>, not <code>expectCall</code> because
   * "expect" appears in the surrounding <code>expecting</code> clause provided by this trait.
   * </p>
   *
   * <p>
   * Moreover, because this method is marked <code>implicit</code>, you will usually be able to simply
   * leave it off. So long as the result of the method call you are expecting doesn't have
   * a method that satisfies the subsequent invocation (such as <code>andReturn</code> in this
   * example), the Scala compiler will invoke <code>call</code> for you
   * implicitly. Here's how that looks:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting {
   *   mock.getName.andReturn("Ben Franklin")
   * }
   * </pre>
   *
   * @param value - the result of invoking a method on mock prior to invoking <code>replay</code>.
   */
  implicit def call[T](value: T): IExpectationSetters[T] = easyMockExpect(value)

  /**
   * Invokes the <code>expectLastCall</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>expect</code> method in Java class <code>org.easymock.EasyMock</code>).
   *
   * <p>
   * This method is provided simply to allow you to avoid repeating "expect" inside an
   * <code>expecting</code> clause. Here's an example that uses the <code>expectLastCall</code> directly
   * to express the expectation that the <code>getName</code> method will be invoked three times
   * on a mock, each time returning <code>"Ben Franklin"</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting {
   *   mock.getName.andReturn("Ben Franklin")
   *   expectLastCall.times(3)
   * }
   * </pre>
   *
   * <p>
   * Using this method, you can compress this to:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting {
   *   mock.getName.andReturn("Ben Franklin")
   *   lastCall.times(3)
   * }
   * </pre>
   */
  def lastCall[T]: IExpectationSetters[T] = expectLastCall()

  /**
   * Invokes the <code>createMock</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>createMock</code> method in Java class <code>org.easymock.classextension.EasyMock</code>).
   *
   * <p>
   * Using the EasyMock API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = createMock(classOf[Collaborator])
   * </pre>
   *
   * <p>
   * Using this method, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock[Collaborator]
   * </pre>
   */
  def mock[T <: AnyRef](implicit manifest: Manifest[T]): T = {
    EasyMock.createMock(manifest.erasure.asInstanceOf[Class[T]])
  }

  /**
   * Invokes the <code>createStrictMock</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>createStrictMock</code> method in Java class <code>org.easymock.classextension.EasyMock</code>).
   *
   * <p>
   * Using the EasyMock API directly, you create a strict mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = createStrictMock(classOf[Collaborator])
   * </pre>
   *
   * <p>
   * Using this trait, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = strictMock[Collaborator]
   * </pre>
   */
  def strictMock[T <: AnyRef](implicit manifest: Manifest[T]): T = {
    EasyMock.createStrictMock(manifest.erasure.asInstanceOf[Class[T]])
  }

  /**
   * Invokes the <code>createNiceMock</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>createNiceMock</code> method in Java class <code>org.easymock.classextension.EasyMock</code>).
   *
   * <p>
   * Using the EasyMock API directly, you create a nice mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = createNiceMock(classOf[Collaborator])
   * </pre>
   *
   * <p>
   * Using this trait, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = niceMock[Collaborator]
   * </pre>
   */
  def niceMock[T <: AnyRef](implicit manifest: Manifest[T]): T = {
    EasyMock.createNiceMock(manifest.erasure.asInstanceOf[Class[T]])
  }

  /**
   * Provides a visual clue to readers of the code that a set of statements are expectations being
   * set on mocks.
   *
   * <p>
   * Using the EasyMock API directly, you set expectations on a mock object with syntax like:
   * </p>
   *
   * <pre class="stHighlight">
   * mockCollaborator.documentAdded("Document")
   * mockCollaborator.documentChanged("Document")
   * expectLastCall().times(3)
   * </pre>
   *
   * <p>
   * This <code>expecting</code> method can make it more obvious which portion of your test code
   * is devoted to setting expectations on mock objects. For example:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting {
   *   mockCollaborator.documentAdded("Document")
   *   mockCollaborator.documentChanged("Document")
   *   lastCall.times(3)
   * }
   * </pre>
   *
   * <p>
   * Using an <code>expecting</code> clause is optional, because it does nothing besides visually indicate
   * which statements are setting expectations on mocks. Note: this trait also provides the <code>lastCall</code>
   * method, which just calls <code>expectLastCall</code>. This allows you to avoid writing "expect" twice.
   * Also, the reason <code>expecting</code> doesn't take a by-name parameter, execute that, then call
   * <code>replay</code> is because you would then need to pass your mock object or objects into
   * <code>expecting</code>. Since you already need to pass the mocks into <code>whenExecuting</code> so
   * that <code>verify</code> can be invoked on them, it yields more concise client code to have
   * <code>whenExecuting</code> invoke <code>replay</code> on the mocks first rather than having
   * <code>expecting</code> invoke <code>replay</code> last.
   * </p>
   */
  def expecting(unused: Any) = ()

  /**
   * Invokes <code>replay</code> on the passed mock object or objects, executes the passed function, then invokes
   * <code>verify</code> on the passed mock object or objects.
   *
   * <p>
   * Once you've set expectations on some mock objects, you must invoke <code>replay</code> on
   * the mocks to indicate you are done setting expectations, and will start using the mocks.
   * After using the mocks, you must invoke <code>verify</code> to check to make sure the mocks
   * were used in accordance with the expectations you set on it. Here's how that looks when you
   * use the EasyMock API directly:
   * </p>
   *
   *
   * <pre class="stHighlight">
   * replay(mock)
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * verify(mock)
   * </pre>
   *
   * <p>
   * This method enables you to use the following, more declarative syntax instead:
   * </p>
   * 
   * <pre class="stHighlight">
   * whenExecuting(mockCollaborator) {
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   * }
   * </pre>
   *
   * <p>
   * If you are working with multiple mock objects at once, you simply pass
   * them all to <code>whenExecuting</code>, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * whenExecuting(mock1, mock2, mock3) {
   *   // ...
   * }
   * </pre>
   *
   * <p>
   * The <code>whenExecuting</code> method will first invoke <code>EasyMock.reply</code>
   * once for each mock you supplied, execute the passed function, then
   * invoke <code>EasyMock.verify</code> once for each mock you supplied. If an exception
   * is thrown by the passed function, <code>whenExecuting</code> will complete abruptly with
   * that same exception without executing verify on any of the mocks.
   * </p>
   *
   * @param mocks one or more mock objects to invoke <code>replay</code> before using and <code>verify</code> after using.
   * @throws IllegalArgumentException if no mocks are passed
   */
  def whenExecuting(mocks: AnyRef*)(fun: => Unit) = {

    require(mocks.length > 0, "Must pass at least one mock to whenExecuting, but mocks.length was 0.") 

    for (m <- mocks)
      EasyMock.replay(m)

    fun

    // Don't put this in a try block, so that if fun throws an exception 
    // it propagates out immediately and shows up as the cause of the failed test
    for (m <- mocks)
      EasyMock.verify(m)
  }

  /**
   * Holder class for a collection of mocks that can be passed implicitly to one form of the
   * overloaded <code>whenExecuting</code> method.
   *
   * @param mocks one or more mock objects that you intend to pass to <code>whenExecuting</code>
   * @throws IllegalArgumentException if no mocks are passed
   */
  case class MockObjects(mocks: AnyRef*) {
    require(mocks.length > 0, "Must pass at least one mock to MockObjects constructor, but mocks.length was 0.") 
  }

  /**
   * Invokes <code>replay</code> on the mock object or objects passed via an implicit parameter,
   * executes the passed function, then invokes <code>verify</code> on the passed mock object or objects.
   *
   * <p>
   * Once you've set expectations on some mock objects, you must invoke <code>replay</code> on
   * the mocks to indicate you are done setting expectations, and will start using the mocks.
   * After using the mocks, you must invoke <code>verify</code> to check to make sure the mocks
   * were used in accordance with the expectations you set on it. Here's how that looks when you
   * use the EasyMock API directly:
   * </p>
   *
   *
   * <pre class="stHighlight">
   * replay(mock)
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * verify(mock)
   * </pre>
   *
   * <p>
   * This method enables you to use the following, more declarative syntax instead:
   * </p>
   * 
   * <pre class="stHighlight">
   * implicit val mocks = MockObjects(mockCollaborator)
   *
   * whenExecuting {
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   * }
   * </pre>
   *
   * <p>
   * If you are working with multiple mock objects at once, you simply pass
   * them all to <code>MockObjects</code>, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * implicit val mocks = MockObjects(mock1, mock2, mock3)
   * </pre>
   *
   * <p>
   * The <code>whenExecuting</code> method will first invoke <code>EasyMock.reply</code>
   * once for each mock you supplied, execute the passed function, then
   * invoke <code>EasyMock.verify</code> once for each mock you supplied. If an exception
   * is thrown by the passed function, <code>whenExecuting</code> will complete abruptly with
   * that same exception without executing verify on any of the mocks.
   * </p>
   */
  def whenExecuting(fun: => Unit)(implicit mocks: MockObjects) {
    whenExecuting(mocks.mocks: _*)(fun)
  }
}

/**
 * Companion object that facilitates the importing of <code>EasyMockSugar</code> members as 
 * an alternative to mixing it in. One use case is to import <code>EasyMockSugar</code> members so you can use
 * them in the Scala interpreter.
 */
// TODO: Fill in an example
object EasyMockSugar extends EasyMockSugar

