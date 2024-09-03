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
package org.scalatest

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class PrettyAstSpec extends AnyFunSpec {

  describe("SharedHelpers.prettifyAst ") {
    it("should print pretty AST") {
      val original =
        "Expr(Apply(Select(Select(This(newTypeName(\"TryIt\")), TermName(\"a\")), TermName(\"$eq$eq\")), " +
          "List(Select(This(newTypeName(\"TryIt\")), TermName(\"b\"))))) " +
          "Expr(Apply(Select(Apply(Select(Select(This(newTypeName(\"TryIt\")), TermName(\"a\")), TermName(\"$eq$eq\")), " +
          "List(Select(This(newTypeName(\"TryIt\")), TermName(\"b\")))), TermName(\"$amp$amp\")), " +
          "List(Apply(Select(Select(This(newTypeName(\"TryIt\")), TermName(\"b\")), TermName(\"$eq$eq\")), " +
          "List(Select(This(newTypeName(\"TryIt\")), TermName(\"c\")))))))"

      SharedHelpers.prettifyAst(original) should be (
        "Expr(\n" +
          "  Apply(\n" +
          "    Select(\n" +
          "      Select(\n" +
          "        This(\n" +
          "          newTypeName(\"TryIt\")\n" +
          "        ),\n" +
          "        TermName(\"a\")\n" +
          "      ),\n" +
          "      TermName(\"$eq$eq\")\n" +
          "    ),\n" +
          "    List(\n" +
          "      Select(\n" +
          "        This(\n" +
          "          newTypeName(\"TryIt\")\n" +
          "        ),\n" +
          "        TermName(\"b\")\n" +
          "      )\n" +
          "    )\n" +
          "  )\n" +
          ")\n" +
          "Expr(\n" +
          "  Apply(\n" +
          "    Select(\n" +
          "      Apply(\n" +
          "        Select(\n" +
          "          Select(\n" +
          "            This(\n" +
          "              newTypeName(\"TryIt\")\n" +
          "            ),\n" +
          "            TermName(\"a\")\n" +
          "          ),\n" +
          "          TermName(\"$eq$eq\")\n" +
          "        ),\n" +
          "        List(\n" +
          "          Select(\n" +
          "            This(\n" +
          "              newTypeName(\"TryIt\")\n" +
          "            ),\n" +
          "            TermName(\"b\")\n" +
          "          )\n" +
          "        )\n" +
          "      ),\n" +
          "      TermName(\"$amp$amp\")\n" +
          "    ),\n" +
          "    List(\n" +
          "      Apply(\n" +
          "        Select(\n" +
          "          Select(\n" +
          "            This(\n" +
          "              newTypeName(\"TryIt\")\n" +
          "            ),\n" +
          "            TermName(\"b\")\n" +
          "          ),\n" +
          "          TermName(\"$eq$eq\")\n" +
          "        ),\n" +
          "        List(\n" +
          "          Select(\n" +
          "            This(\n" +
          "              newTypeName(\"TryIt\")\n" +
          "            ),\n" +
          "            TermName(\"c\")\n" +
          "          )\n" +
          "        )\n" +
          "      )\n" +
          "    )\n" +
          "  )\n" +
          ")\n"
      )
    }

    it("should print pretty AST correctly also when '(' or ')' is in strings") {
      val original =
        "Expr(Apply(Select(Ident(scala.Predef), TermName(\"println\")), List(Literal(Constant(\"Hello (chua\")))))"
      SharedHelpers.prettifyAst(original) should be (
        "Expr(\n" +
          "  Apply(\n" +
          "    Select(\n" +
          "      Ident(scala.Predef),\n" +
          "      TermName(\"println\")\n" +
          "    ),\n" +
          "    List(\n" +
          "      Literal(\n" +
          "        Constant(\"Hello (chua\")\n" +
          "      )\n" +
          "    )\n" +
          "  )\n" +
          ")\n"
      )
    }
  }

}