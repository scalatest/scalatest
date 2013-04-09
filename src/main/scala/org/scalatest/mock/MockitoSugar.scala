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
import org.mockito.Mockito.{mock => mockitoMock}
import reflect.Manifest
import org.mockito.stubbing.Answer
import org.mockito.MockSettings

/**
 * Trait that provides some basic syntax sugar for <a href="http://mockito.org/" target="_blank">Mockito</a>.
 *
 * <p>
 * Using the Mockito API directly, you create a mock with:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = mock(classOf[Collaborator])
 * </pre>
 *
 * <p>
 * Using this trait, you can shorten that to:
 * </p>
 *
 * <pre class="stHighlight">
 * val mockCollaborator = mock[Collaborator]
 * </pre>
 *
 * <p>
 * This trait also provides shorthands for the three other (non-deprecated) overloaded <code>mock</code> methods,
 * which allow you to pass in a default answer, a name, or settings.
 * </p>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
trait MockitoSugar {

  /**
   * Invokes the <code>mock(classToMock: Class[T])</code> method on the <code>Mockito</code> companion object (<em>i.e.</em>, the
   * static <code>mock(java.lang.Class<T> classToMock)</code> method in Java class <code>org.mockito.Mockito</code>).
   *
   * <p>
   * Using the Mockito API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock(classOf[Collaborator])
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
    mockitoMock(manifest.erasure.asInstanceOf[Class[T]])
  }
  
  /**
   * Invokes the <code>mock(classToMock: Class[T], defaultAnswer: Answer[_])</code> method on the <code>Mockito</code> companion object (<em>i.e.</em>, the
   * static <code>mock(java.lang.Class<T> classToMock, org.mockito.stubbing.Answer defaultAnswer)</code> method in Java class <code>org.mockito.Mockito</code>).
   *
   * <p>
   * Using the Mockito API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock(classOf[Collaborator], defaultAnswer)
   * </pre>
   *
   * <p>
   * Using this method, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock[Collaborator](defaultAnswer)
   * </pre>
   */
  def mock[T <: AnyRef](defaultAnswer: Answer[_])(implicit manifest: Manifest[T]): T = {
    mockitoMock(manifest.erasure.asInstanceOf[Class[T]], defaultAnswer)
  }
  
  /**
   * Invokes the <code>mock(classToMock: Class[T], mockSettings: MockSettings)</code> method on the <code>Mockito</code> companion object (<em>i.e.</em>, the
   * static <code>mock(java.lang.Class<T> classToMock, org.mockito.MockSettings mockSettings)</code> method in Java class <code>org.mockito.Mockito</code>).
   *
   * <p>
   * Using the Mockito API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock(classOf[Collaborator], mockSettings)
   * </pre>
   *
   * <p>
   * Using this method, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock[Collaborator](mockSettings)
   * </pre>
   */
  def mock[T <: AnyRef](mockSettings: MockSettings)(implicit manifest: Manifest[T]): T = {
    mockitoMock(manifest.erasure.asInstanceOf[Class[T]], mockSettings)
  }
  
  /**
   * Invokes the <code>mock(classToMock: Class[T], name: String)</code> method on the <code>Mockito</code> companion object (<em>i.e.</em>, the
   * static <code>mock(java.lang.Class<T> classToMock, java.lang.String name)</code> method in Java class <code>org.mockito.Mockito</code>).
   *
   * <p>
   * Using the Mockito API directly, you create a mock with:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock(classOf[Collaborator], name)
   * </pre>
   *
   * <p>
   * Using this method, you can shorten that to:
   * </p>
   *
   * <pre class="stHighlight">
   * val mockCollaborator = mock[Collaborator](name)
   * </pre>
   */
  def mock[T <: AnyRef](name: String)(implicit manifest: Manifest[T]): T = {
    mockitoMock(manifest.erasure.asInstanceOf[Class[T]], name)
  }
}

/**
 * Companion object that facilitates the importing of <code>MockitoSugar</code> members as 
 * an alternative to mixing it in. One use case is to import <code>MockitoSugar</code> members so you can use
 * them in the Scala interpreter.
 */
// TODO: Fill in an example
object MockitoSugar extends MockitoSugar

