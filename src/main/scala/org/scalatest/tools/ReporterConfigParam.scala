/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.tools

import org.scalatest._

private[tools] sealed abstract class ReporterConfigParam(val character: String)

private[tools] case object FilterTestStarting extends ReporterConfigParam("N")
private[tools] case object FilterTestSucceeded extends ReporterConfigParam("C")
private[tools] case object FilterTestIgnored extends ReporterConfigParam("X")
private[tools] case object FilterTestPending extends ReporterConfigParam("E")
private[tools] case object FilterTestCanceled extends ReporterConfigParam("A")
private[tools] case object FilterSuiteStarting extends ReporterConfigParam("H")
private[tools] case object FilterSuiteCompleted extends ReporterConfigParam("L")
private[tools] case object FilterInfoProvided extends ReporterConfigParam("O")
private[tools] case object FilterScopeOpened extends ReporterConfigParam("P")
private[tools] case object FilterScopeClosed extends ReporterConfigParam("Q")
private[tools] case object FilterScopePending extends ReporterConfigParam("R")
private[tools] case object FilterMarkupProvided extends ReporterConfigParam("M")
private[tools] case object PresentShortStackTraces extends ReporterConfigParam("S")
private[tools] case object PresentFullStackTraces extends ReporterConfigParam("F")
private[tools] case object PresentWithoutColor extends ReporterConfigParam("W")
private[tools] case object PresentAllDurations extends ReporterConfigParam("D")
private[tools] case object PresentUnformatted extends ReporterConfigParam("U")
