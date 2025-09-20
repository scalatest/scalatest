/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalatest.tools

import scala.collection.mutable
import scala.collection.Seq
import scala.xml._

/**
 * The `Utility` object provides utility functions for processing instances
 * of bound and not bound XML classes, as well as escaping text nodes.
 *
 * @author Burak Emir
 */
private[tools] object XmlUtility {
  // helper for the extremely oft-repeated sequence of creating a
  // StringBuilder, passing it around, and then grabbing its String.
  private[tools] def sbToString(f: (StringBuilder) => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  private[tools] def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]

  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): Unit = {
    if (children.isEmpty) return
    else if (children forall isAtomAndNotText) { // add space
      val it = children.iterator
      val f = it.next()
      serialize(f, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      while (it.hasNext) {
        val x = it.next()
        sb.append(' ')
        serialize(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      }
    } else children foreach { serialize(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }  
  }

  /**
   * Serialize an XML Node to a StringBuilder.
   *
   * This is essentially a minor rework of `toXML` that can't have the same name due to an unfortunate
   * combination of named/default arguments and overloading.
   *
   * @todo use a Writer instead
   */
  def serialize(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): StringBuilder =
    {
      x match {
        case c: Comment                   => if (!stripComments) c buildString sb; sb
        case s: SpecialNode               => s buildString sb
        case g: Group                     =>
          for (c <- g.nodes) serialize(c, g.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags); sb
        case el: Elem =>
          // print tag with namespace declarations
          sb.append('<')
          el.nameToString(sb)
          if (el.attributes ne null) el.attributes.buildString(sb)
          el.scope.buildString(sb, pscope)
          if (el.child.isEmpty &&
            (minimizeTags == MinimizeMode.Always ||
              (minimizeTags == MinimizeMode.Default && el.minimizeEmpty))) {
            // no children, so use short form: <xyz .../>
            sb.append("/>")
          } else {
            // children, so use long form: <xyz ...>...</xyz>
            sb.append('>')
            sequenceToXML(el.child, el.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
            sb.append("</")
            el.nameToString(sb)
            sb.append('>')
          }
        case _ => throw new IllegalArgumentException("Don't know how to serialize a " + x.getClass.getName)
      }
    }
}