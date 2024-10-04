/*
 * Copyright 2001-2024 Artima, Inc.
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

import java.net.URL

/**
 * This file has types that are used in parsing command line arguments to Runner.
 *
 * @author Bill Venners
 */
private[tools] sealed abstract class ReporterConfiguration extends Product with Serializable

private[tools] case class GraphicReporterConfiguration(configSet: Set[ReporterConfigParam]) extends ReporterConfiguration
private[tools] case class StandardOutReporterConfiguration(configSet: Set[ReporterConfigParam]) extends ReporterConfiguration
private[tools] case class StandardErrReporterConfiguration(configSet: Set[ReporterConfigParam]) extends ReporterConfiguration
private[tools] case class FileReporterConfiguration(configSet: Set[ReporterConfigParam], fileName: String) extends ReporterConfiguration
private[tools] case class MemoryReporterConfiguration(fileName: String) extends ReporterConfiguration
private[tools] case class JunitXmlReporterConfiguration(configSet: Set[ReporterConfigParam], fileName: String) extends ReporterConfiguration
private[tools] case class DashboardReporterConfiguration(configSet: Set[ReporterConfigParam], fileName: String, numOldFilesToKeep: Int) extends ReporterConfiguration
private[tools] case class XmlReporterConfiguration(configSet: Set[ReporterConfigParam], fileName: String) extends ReporterConfiguration
private[tools] case class HtmlReporterConfiguration(configSet: Set[ReporterConfigParam], directory: String, cssFileName: Option[URL]) extends ReporterConfiguration
private[tools] case class CustomReporterConfiguration(configSet: Set[ReporterConfigParam], reporterClass: String) extends ReporterConfiguration
private[tools] case class XmlSocketReporterConfiguration(host: String, port: Int) extends ReporterConfiguration
private[tools] case class SocketReporterConfiguration(host: String, port: Int) extends ReporterConfiguration

// If there were no fileReporterSpecList or customReporterSpecList specified, you get Nil
// If there were no graphicReporterSpec, standardOutReporterSpec, or standardErrReporterSpec, you get None
private[tools] case class ReporterConfigurations(
  val graphicReporterConfiguration: Option[GraphicReporterConfiguration],
  val fileReporterConfigurationList: List[FileReporterConfiguration],
  val memoryReporterConfigurationList: List[MemoryReporterConfiguration],
  val junitXmlReporterConfigurationList: List[JunitXmlReporterConfiguration],
  //val dashboardReporterConfigurationList: List[DashboardReporterConfiguration],
  //val xmlReporterConfigurationList: List[XmlReporterConfiguration],
  val standardOutReporterConfiguration: Option[StandardOutReporterConfiguration],
  val standardErrReporterConfiguration: Option[StandardErrReporterConfiguration],
  val htmlReporterConfigurationList: List[HtmlReporterConfiguration],
  val customReporterConfigurationList: List[CustomReporterConfiguration], 
  val xmlSocketReporterConfigurationList: List[XmlSocketReporterConfiguration], 
  val socketReporterConfigurationList: List[SocketReporterConfiguration]
) extends Seq[ReporterConfiguration] {

  val reporterConfigurationList =
    graphicReporterConfiguration.toList ++
    fileReporterConfigurationList ++
    memoryReporterConfigurationList ++
    junitXmlReporterConfigurationList ++
    //dashboardReporterConfigurationList ++
    //xmlReporterConfigurationList ++
    standardOutReporterConfiguration.toList ++
    standardErrReporterConfiguration.toList ++
    htmlReporterConfigurationList ++
    customReporterConfigurationList ++
    xmlSocketReporterConfigurationList ++
    socketReporterConfigurationList

  // Need to add the null pointer checks, or later, NotNull

  override def length = reporterConfigurationList.length
  // override def elements = reporterConfigurationList.iterator
  override def iterator = reporterConfigurationList.iterator // For 2.8
  override def apply(i: Int) = reporterConfigurationList(i)
}

