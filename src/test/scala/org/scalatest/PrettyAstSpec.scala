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

import Matchers._

class PrettyAstSpec extends Spec {

  def `SharedHelpers.prettifyAst should print pretty AST` {
    val original =
      "Expr(Apply(Select(Select(This(newTypeName(\"TryIt\")), newTermName(\"a\")), newTermName(\"$eq$eq\")), " +
        "List(Select(This(newTypeName(\"TryIt\")), newTermName(\"b\"))))) " +
        "Expr(Apply(Select(Apply(Select(Select(This(newTypeName(\"TryIt\")), newTermName(\"a\")), newTermName(\"$eq$eq\")), " +
        "List(Select(This(newTypeName(\"TryIt\")), newTermName(\"b\")))), newTermName(\"$amp$amp\")), " +
        "List(Apply(Select(Select(This(newTypeName(\"TryIt\")), newTermName(\"b\")), newTermName(\"$eq$eq\")), " +
        "List(Select(This(newTypeName(\"TryIt\")), newTermName(\"c\")))))))"

    SharedHelpers.prettifyAst(original) should be (
      "Expr(\n" +
        "  Apply(\n" +
        "    Select(\n" +
        "      Select(\n" +
        "        This(\n" +
        "          newTypeName(\"TryIt\")\n" +
        "        ),\n" +
        "        newTermName(\"a\")\n" +
        "      ),\n" +
        "      newTermName(\"$eq$eq\")\n" +
        "    ),\n" +
        "    List(\n" +
        "      Select(\n" +
        "        This(\n" +
        "          newTypeName(\"TryIt\")\n" +
        "        ),\n" +
        "        newTermName(\"b\")\n" +
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
        "            newTermName(\"a\")\n" +
        "          ),\n" +
        "          newTermName(\"$eq$eq\")\n" +
        "        ),\n" +
        "        List(\n" +
        "          Select(\n" +
        "            This(\n" +
        "              newTypeName(\"TryIt\")\n" +
        "            ),\n" +
        "            newTermName(\"b\")\n" +
        "          )\n" +
        "        )\n" +
        "      ),\n" +
        "      newTermName(\"$amp$amp\")\n" +
        "    ),\n" +
        "    List(\n" +
        "      Apply(\n" +
        "        Select(\n" +
        "          Select(\n" +
        "            This(\n" +
        "              newTypeName(\"TryIt\")\n" +
        "            ),\n" +
        "            newTermName(\"b\")\n" +
        "          ),\n" +
        "          newTermName(\"$eq$eq\")\n" +
        "        ),\n" +
        "        List(\n" +
        "          Select(\n" +
        "            This(\n" +
        "              newTypeName(\"TryIt\")\n" +
        "            ),\n" +
        "            newTermName(\"c\")\n" +
        "          )\n" +
        "        )\n" +
        "      )\n" +
        "    )\n" +
        "  )\n" +
        ")\n"
    )
  }

}