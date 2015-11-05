/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest.events

/**
 * Location in source code indicating where in the source code an event originated.
 */
sealed abstract class Location

/**
 * The location in a source file where the class whose by the fully qualified name
 * is passed as <code>className</code> is declared.
 *
 * @param className the fully qualified class name
 */
final case class TopOfClass(className: String) extends Location

/**
 * The location in a source file where the method identified by the passed <code>methodId</code> 
 * in the class whose fully qualified name is pased as <code>className</code> is declared.  
 * The methodId is obtained by calling <code>toGenericString</code> on the <code>java.lang.reflect.Method</code> 
 * object representing the method.
 *
 * @param className the fully qualified class name
 * @param methodId the method ID, obtained by calling <code>toGenericString</code> on the <code>java.lang.reflect.Method</code> object representing the method
 */
final case class TopOfMethod(className: String, methodId: String) extends Location

/**
 * An arbitrary line number in a named source file.
 *
 * @param lineNumber the line number
 * @param fileName the source's filename
 */
final case class LineInFile(lineNumber: Int, fileName: String) extends Location

/**
 * Indicates the location should be taken from the stack depth exception, included elsewhere in 
 * the event that contained this location.
 */
final case object SeeStackDepthException extends Location

