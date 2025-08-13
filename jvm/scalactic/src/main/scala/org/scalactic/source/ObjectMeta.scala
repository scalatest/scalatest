/*
 * Copyright 2001-2025 Artima, Inc.
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

import scala.util.{Failure, Try, Success}

trait ObjectMeta {

  def fieldNames: scala.collection.immutable.IndexedSeq[String]

  def hasField(name: String): Boolean = fieldNames.contains(name)

  def value(name: String): Any

  def typeName(name: String): String

  def shortTypeName(name: String): String

}

object ObjectMeta {

  def objectMetaUsingJavaReflection(v: Any): ObjectMeta =
    new ObjectMeta {
      val primaryConstructorParameterNames = v.getClass.getDeclaredConstructors.toList.sortBy(_.getParameterCount).head.getParameters.map(_.getName)
      lazy val privFields = v.getClass.getDeclaredFields.filter(!_.isAccessible).map(_.getName)

      val declaredMethods = v.getClass.getDeclaredMethods

      lazy val fieldNames = {
        declaredMethods.filter { m =>
          m.getParameterTypes.isEmpty && 
          privFields.contains(m.getName) && 
          (primaryConstructorParameterNames.contains(m.getName) || 
           java.lang.reflect.Modifier.isPublic(m.getModifiers) || 
           declaredMethods.find(_.getName.startsWith(s"${m.getName}$$")).isDefined  // for Scala 2.11's case class private field, we can remove this when we drop support for Scala 2.11.
          ) && 
          m.getName != "$outer"
        }.map { f =>
          if (f.getName.endsWith("$mcI$sp"))
            f.getName.dropRight(7)
          else
            f.getName
        }.toVector
      }

      def value(name: String): Any = {
        val fieldTry = Try(v.getClass.getDeclaredMethod(name))
          .recoverWith {
            case e: NoSuchMethodException =>
              Try(v.getClass.getDeclaredMethod(name + "$mcI$sp"))
                .recoverWith {
                  case e: NoSuchMethodException =>
                    Failure(new IllegalArgumentException("'" + name + "' is not attribute for this instance."))
                }
          }

        fieldTry match {
          case Failure(e) => throw e
          case Success(field) =>
            try {
              field.invoke(v)
            } catch {
              case e: IllegalAccessException =>
                field.setAccessible(true)
                val value = field.invoke(v)
                field.setAccessible(false)
                value
            }
        }
      }

      def typeName(name: String): String = value(name).getClass.getName

      def shortTypeName(name: String): String = value(name).getClass.getSimpleName
    }

  def apply(v: Any): ObjectMeta = objectMetaUsingJavaReflection(v)

}