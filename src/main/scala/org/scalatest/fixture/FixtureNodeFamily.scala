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
package org.scalatest.fixture

import org.scalatest._

private[fixture] object FixtureNodeFamily  {

  sealed abstract class Node(parentOption: Option[Branch])

  abstract class Branch(parentOption: Option[Branch]) extends Node(parentOption) {
    var subNodes: List[Node] = Nil
  }

  class Trunk extends Branch(None)

  case class FixtureTestLeaf[FixtureParam](
    parent: Branch,
    testName: String,
    specText: String,
    f: FixtureParam => Any
  ) extends Node(Some(parent))

  case class InfoLeaf(parent: Branch, message: String) extends Node(Some(parent))

  case class DescriptionBranch(
    parent: Branch,
    descriptionName: String
  ) extends Branch(Some(parent))

  case class VerbBranch(
    parent: Branch,
    descriptionName: String,
    verb: String
  ) extends Branch(Some(parent))

  // This is used to get a complete test name. If a VerbBranch, it looks for hard coded (when and if
  // it finds it puts a trailing close paren in there. It also includes the verb in the prefix,
  // because that needs to be in the test name
  private[scalatest] def getPrefix(branch: Branch): String = {
    branch match {
      case _: Trunk => ""
      // Call to getPrefix is not tail recursive, but I don't expect
      // the describe nesting to be very deep (famous last words).
      case DescriptionBranch(parent, descriptionName) =>
        Resources("prefixSuffix", getPrefix(parent), descriptionName)
      case VerbBranch(parent, descriptionName, verb) =>
        val prefix = getPrefix(parent)
        val suffix = if (prefix.indexOf(" (when ") != -1) ")" else ""
        val withoutVerb = Resources("prefixSuffix", prefix, descriptionName + suffix)
        Resources("prefixSuffix", withoutVerb, verb)
    }
  }

  // This is used for descriptions. If a VerbBranch, it looks for hard coded (when and if
  // it finds it puts a trailing close paren in there. It does not put the verb in there, because
  // that needs to go into the first part of the specText that gets printed out for a test.
  private[scalatest] def getPrefixWithoutVerb(branch: Branch): String = {
    branch match {
      case _: Trunk => ""
      // Call to getPrefix is not tail recursive, but I don't expect
      // the describe nesting to be very deep (famous last words).
      case DescriptionBranch(parent, descriptionName) =>
        Resources("prefixSuffix", getPrefix(parent), descriptionName)
      case VerbBranch(parent, descriptionName, _) =>
        val prefix = getPrefix(parent)
        val suffix = if (prefix.indexOf(" (when ") != -1) ")" else "" // TODO: Search up the tree for a when clause?
        Resources("prefixSuffix", prefix, descriptionName + suffix)
    }
  }

  // This one is for formatted specText, like:
  // - can do something
  // So it goes up to the verb and stops
  private[scalatest] def getFormattedSpecTextPrefix(branch: Branch): String = {
    branch match {
      case _: Trunk => ""
      // Call to getTestPrefix is not tail recursive, but I don't expect
      // the describe nesting to be very deep (famous last words).
      case DescriptionBranch(parent, descriptionName) =>
        Resources("prefixSuffix", getFormattedSpecTextPrefix(parent), descriptionName)
      case VerbBranch(parent, descriptionName, verb) => verb
    }
  }

  private[scalatest] def getTestName(specText: String, parent: Branch): String = {
    val prefix = getPrefix(parent).trim
    if (prefix.isEmpty) {
      // class MySpec extends Spec {
      //   it("should pop when asked") {}
      // }
      // Should yield: "should pop when asked"
      specText
    }
    else {
      // class MySpec extends Spec {
      //   describe("A Stack") {
      //     it("must pop when asked") {}
      //   }
      // }
      // Should yield: "A Stack must pop when asked"
      Resources("prefixSuffix", prefix, specText)
    }
  }
}
