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
import org.jmock.api.ExpectationError
import org.jmock.{Expectations, Mockery}
import org.jmock.lib.legacy.ClassImposteriser
import scala.reflect.Manifest

/**
 * Class that wraps and manages the lifecycle of a single <code>org.jmock.Mockery</code> context object,
 * provides some basic syntax sugar for using <a href="http://www.jmock.org/" target="_blank">JMock</a>
 * in Scala.
 *
 * <p>
 * Using the JMock API directly, you first need a <code>Mockery</code> context object:
 * </p>
 *
 * <pre class="stHighlight">
 * val context = new Mockery
 * </pre>
 *
 * <p>
 * <code>JMockCycle</code> uses jMock's <code>ClassImposterizer</code> to support mocking of classes, so the following line
 * would also be needed if you wanted that functionality as well:
 * </p>
 *
 * <pre class="stHighlight">
 * context.setImposteriser(ClassImposteriser.INSTANCE)
 * </pre>
 *
 * <p>
 * When using this class, you would instead create an instance of this class (which will create and
 * wrap a <code>Mockery</code> object) and import its members, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val cycle = new JMockCycle
 * import cycle._
 * </pre>
 *
 * <p>
 * Using the JMock API directly, you would create a mock object like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = context.mock(classOf[Collaborator])
 * </pre>
 *
 * <p>
 * Having imported the members of an instance of this class, you can shorten that to:
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
 * context.checking(
 *   new Expectations() {
 *     oneOf (mockCollaborator).documentAdded("Document")
 *     exactly(3).of (mockCollaborator).documentChanged("Document")
 *    }
 *  )
 * </pre>
 *
 * <p>
 * Having imported the members of an instance of this class, you can shorten this step to:
 * </p>
 *
 * <pre class="stHighlight">
 * expecting { e => import e._
 *   oneOf (mockCollaborator).documentAdded("Document")
 *   exactly(3).of (mockCollaborator).documentChanged("Document")
 * }
 * </pre>
 *
 * <p>
 * The <code>expecting</code> method will create a new <code>Expectations</code> object, pass it into
 * the function you provide, which sets the expectations. After the function returns, the <code>expecting</code>
 * method will pass the <code>Expectations</code> object to the <code>checking</code>
 * method of its internal <code>Mockery</code> context.
 * </p>
 *
 * <p>
 * The <code>expecting</code> method passes an instance of class
 * <code>org.scalatest.mock.JMockExpectations</code> to the function you pass into
 * <code>expectations</code>. <code>JMockExpectations</code> extends <code>org.jmock.Expectations</code> and
 * adds several overloaded <code>withArg</code> methods. These <code>withArg</code> methods simply
 * invoke corresponding <code>with</code> methods on themselves. Because <code>with</code> is
 * a keyword in Scala, to invoke these directly you must surround them in back ticks, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * oneOf (mockCollaborator).documentAdded(`with`("Document"))
 * </pre>
 *
 * <p>
 * By importing the members of the passed <code>JMockExpectations</code> object, you can
 * instead call <code>withArg</code> with no back ticks needed:
 * </p>
 *
 * <pre class="stHighlight">
 * oneOf (mockCollaborator).documentAdded(withArg("Document"))
 * </pre>
 *
 * <p>
 * Once you've set expectations on the mock objects, when using the JMock API directly, you use the mock, then invoke
 * <code>assertIsSatisfied</code> on the <code>Mockery</code> context to make sure the mock
 * was used in accordance with the expectations you set on it. Here's how that looks:
 * </p>
 *
 * <pre class="stHighlight">
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * classUnderTest.addDocument("Document", new Array[Byte](0))
 * context.assertIsSatisfied()
 * </pre>
 *
 * <p>
 * This class enables you to use the following, more declarative syntax instead:
 * </p>
 *
 * <pre class="stHighlight">
 * whenExecuting {
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 *   classUnderTest.addDocument("Document", new Array[Byte](0))
 * }
 * </pre>
 *
 * <p>
 * The <code>whenExecuting</code> method will execute the passed function, then
 * invoke <code>assertIsSatisfied</code> on its internal <code>Mockery</code>
 * context object.
 * </p>
 *
 * <p>
 * To summarize, here's what a typical test using <code>JMockCycle</code> looks like:
 * </p>
 *
 * <pre class="stHighlight">
 * val cycle = new JMockCycle
 * import cycle._
 *
 * val mockCollaborator = mock[Collaborator]
 *
 * expecting { e => import e._
 *   oneOf (mockCollaborator).documentAdded("Document")
 *   exactly(3).of (mockCollaborator).documentChanged("Document")
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
 * ScalaTest also provides a <a href="JMockCycleFixture.html"><code>JMockCycleFixture</code></a> trait, which
 * will pass a new <code>JMockCycle</code> into each test that needs one.
 * </p>
 *
 * @author Bill Venners
 */
final class JMockCycle {

  private val context = new Mockery
  context.setImposteriser(ClassImposteriser.INSTANCE)

  /**
   * Invokes the <code>mock</code> method on this <code>JMockCycle</code>'s internal
   * <code>Mockery</code> context object, passing in a class instance for the
   * specified type parameter.
   *
   * <p>
   * Using the JMock API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = context.mock(classOf[Collaborator])
   * </pre>
   *
   * <p>
   * Having imported the members of an instance of this class, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock[Collaborator]
   * </pre>
   */
  def mock[T <: AnyRef](implicit manifest: Manifest[T]): T = {
    context.mock(manifest.erasure.asInstanceOf[Class[T]])
  }

  /**
   * Sets expectations on mock objects.
   *
   * <p>
   * After creating mocks, you set expectations on them, using syntax like this:
   * </p>
   *
   * <pre class="stHighlight">
   * context.checking(
   *   new Expectations() {
   *     oneOf (mockCollaborator).documentAdded("Document")
   *     exactly(3).of (mockCollaborator).documentChanged("Document")
   *    }
   *  )
   * </pre>
   *
   * <p>
   * Having imported the members of an instance of this class, you can shorten this step to:
   * </p>
   *
   * <pre class="stHighlight">
   * expecting { e => import e._
   *   oneOf (mockCollaborator).documentAdded("Document")
   *   exactly(3).of (mockCollaborator).documentChanged("Document")
   * }
   * </pre>
   *
   * <p>
   * The <code>expecting</code> method will create a new <code>Expectations</code> object, pass it into
   * the function you provide, which sets the expectations. After the function returns, the <code>expecting</code>
   * method will pass the <code>Expectations</code> object to the <code>checking</code>
   * method of its internal <code>Mockery</code> context.
   * </p>
   *
   * <p>
   * This method passes an instance of class <code>org.scalatest.mock.JMockExpectations</code> to the
   * passed function. <code>JMockExpectations</code> extends <code>org.jmock.Expectations</code> and
   * adds several overloaded <code>withArg</code> methods. These <code>withArg</code> methods simply
   * invoke corresponding <code>with</code> methods on themselves. Because <code>with</code> is
   * a keyword in Scala, to invoke these directly you must surround them in back ticks, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * oneOf (mockCollaborator).documentAdded(`with`("Document"))
   * </pre>
   *
   * <p>
   * By importing the members of the passed <code>JMockExpectations</code> object, you can
   * instead call <code>withArg</code> with no back ticks needed:
   * </p>
   *
   * <pre class="stHighlight">
   * oneOf (mockCollaborator).documentAdded(withArg("Document"))
   * </pre>
   *
   * @param fun a function that sets expectations on the passed <code>JMockExpectations</code>
   *    object
   */
  def expecting(fun: JMockExpectations => Unit) {
    val e = new JMockExpectations
    fun(e)
    context.checking(e)
  }

  /**
   * Executes code using mocks with expectations set.
   * 
   * <p>
   * Once you've set expectations on the mock objects, when using the JMock API directly, you use the mock, then invoke
   * <code>assertIsSatisfied</code> on the <code>Mockery</code> context to make sure the mock
   * was used in accordance with the expectations you set on it. Here's how that looks:
   * </p>
   *
   * <pre class="stHighlight">
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * classUnderTest.addDocument("Document", new Array[Byte](0))
   * context.assertIsSatisfied()
   * </pre>
   *
   * <p>
   * This class enables you to use the following, more declarative syntax instead:
   * </p>
   *
   * <pre class="stHighlight">
   * whenExecuting {
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   *   classUnderTest.addDocument("Document", new Array[Byte](0))
   * }
   * </pre>
   *
   * <p>
   * The <code>whenExecuting</code> method will execute the passed function, then
   * invoke <code>assertIsSatisfied</code> on its internal <code>Mockery</code>
   * context object.
   * </p>
   *
   * @param fun the code to execute under previously set expectations
   * @throws org.mock.ExpectationError if an expectation is not met
   */
  def whenExecuting(fun: => Unit) = {
    fun
    context.assertIsSatisfied()
  }
}
