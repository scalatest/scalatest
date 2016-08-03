/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalactic.source

trait CaseClassMeta {

  def caseAccessorNames: scala.collection.immutable.IndexedSeq[String]

  def value(name: String): Any

  def typeName(name: String): String

  def shortTypeName(name: String): String

}

object CaseClassMeta {

  import reflect.runtime.universe._

  def isCaseClass(value: Any): Boolean = {
    val typeMirror = runtimeMirror(value.getClass.getClassLoader)
    val instanceMirror = typeMirror.reflect(value)
    val symbol: ClassSymbol = instanceMirror.symbol
    symbol.isCaseClass
  }

  def apply(v: Any): CaseClassMeta = {

    val typeMirror = runtimeMirror(v.getClass.getClassLoader)
    val instanceMirror = typeMirror.reflect(v)
    val symbol: ClassSymbol = instanceMirror.symbol

    def isCaseAccessor(s: Symbol): Boolean = {
      s match {
        case m: MethodSymbol if m.isCaseAccessor =>
          try {
            val fieldMirror = instanceMirror.reflectField(m.asTerm)
            fieldMirror.get
            true
          }
          catch {
            case e: Throwable => false
          }

        case _ => false
      }
    }

    if (symbol.isCaseClass) {
      new CaseClassMeta {

        val caseAccessorSymbols = symbol.toType.declarations.filter(isCaseAccessor)

        lazy val caseAccessorNames = {
          caseAccessorSymbols.map(_.name.toString).toVector
        }

        def value(name: String): Any = {
          caseAccessorSymbols.find(s => s.name.toString == name) match {
            case Some(fieldSymbol) =>
              val fieldMirror = instanceMirror.reflectField(fieldSymbol.asTerm)
              fieldMirror.get

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not a case accessor for this instance of case class.")
          }
        }

        def typeName(name: String): String = {
          caseAccessorSymbols.find(s => s.name.toString == name) match {
            case Some(fieldSymbol) =>
              // This is a safe cast
              fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.fullName

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not a case accessor for this instance of case class.")
          }
        }

        def shortTypeName(name: String): String = {
          caseAccessorSymbols.find(s => s.name.toString == name) match {
            case Some(fieldSymbol) =>
              // This is a safe cast
              fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.name.decoded

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not a case accessor for this instance of case class.")
          }
        }
      }
    }
    else
      throw new IllegalArgumentException(symbol.fullName + " is not a case class.")
  }

}