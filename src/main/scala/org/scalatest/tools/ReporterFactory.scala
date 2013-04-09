package org.scalatest.tools
import org.scalatest.Reporter
import org.scalatest.Resources
import org.scalatest.DispatchReporter
import java.net.URL

private[scalatest] class ReporterFactory {
  
  private[tools] def configSetMinusNonFilterParams(configSet: Set[ReporterConfigParam]) =
    (((configSet - PresentShortStackTraces) - PresentFullStackTraces) - PresentWithoutColor) - PresentAllDurations
    
  private[tools] def getCustomReporter(reporterClassName: String, loader: ClassLoader, argString: String): Reporter = {
    try {
      val reporterClass: java.lang.Class[_] = loader.loadClass(reporterClassName) 
      reporterClass.newInstance.asInstanceOf[Reporter]
    }    // Could probably catch ClassCastException too
    catch {
      case e: ClassNotFoundException => {

        val msg1 = Resources("cantLoadReporterClass", reporterClassName)
        val msg2 = Resources("probarg", argString)
        val msg = msg1 + "\n" + msg2
    
        val iae = new IllegalArgumentException(msg)
        iae.initCause(e)
        throw iae
      }
      case e: InstantiationException => {

        val msg1 = Resources("cantInstantiateReporter", reporterClassName)
        val msg2 = Resources("probarg", argString)
        val msg = msg1 + "\n" + msg2
    
        val iae = new IllegalArgumentException(msg)
        iae.initCause(e)
        throw iae
      }
      case e: IllegalAccessException => {

        val msg1 = Resources("cantInstantiateReporter", reporterClassName)
        val msg2 = Resources("probarg", argString)
        val msg = msg1 + "\n" + msg2
    
        val iae = new IllegalArgumentException(msg)
        iae.initCause(e)
        throw iae
      }
    }
  }
  
  protected def createStandardOutReporter(configSet: Set[ReporterConfigParam]) = {
    if (configSetMinusNonFilterParams(configSet).isEmpty)
      new StandardOutReporter(
        configSet.contains(PresentAllDurations),
        !configSet.contains(PresentWithoutColor),
        configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
        configSet.contains(PresentUnformatted)
      )
    else
      new FilterReporter(
        new StandardOutReporter(
          configSet.contains(PresentAllDurations),
          !configSet.contains(PresentWithoutColor),
          configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
          configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
          configSet.contains(PresentUnformatted)
        ),
        configSet
      )
  }
  
  protected def createStandardErrReporter(configSet: Set[ReporterConfigParam]) = {
    if (configSetMinusNonFilterParams(configSet).isEmpty)
      new StandardErrReporter(
        configSet.contains(PresentAllDurations),
        !configSet.contains(PresentWithoutColor),
        configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
        configSet.contains(PresentUnformatted)
      )
      else
        new FilterReporter(
          new StandardErrReporter(
            configSet.contains(PresentAllDurations),
            !configSet.contains(PresentWithoutColor),
            configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
            configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
            configSet.contains(PresentUnformatted)
          ),
          configSet
        )
  }
  
  protected def createFileReporter(configSet: Set[ReporterConfigParam], fileName: String) = {
    if (configSetMinusNonFilterParams(configSet).isEmpty)
      new FileReporter(
        fileName,
        configSet.contains(PresentAllDurations),
        !configSet.contains(PresentWithoutColor),
        configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
        configSet.contains(PresentUnformatted)
      )
      else
        new FilterReporter(
          new FileReporter(
            fileName,
            configSet.contains(PresentAllDurations),
            !configSet.contains(PresentWithoutColor),
            configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
            configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
            configSet.contains(PresentUnformatted)
          ),
          configSet
        )
  }
  
  protected def createXmlReporter(configSet: Set[ReporterConfigParam], directory: String) = {
    new XmlReporter(directory)
  }
  
  protected def createHtmlReporter(configSet: Set[ReporterConfigParam], directory: String, cssUrl: Option[URL], resultHolder: Option[SuiteResultHolder]) = {
    if (configSetMinusNonFilterParams(configSet).isEmpty)
      new HtmlReporter(
        directory,
        configSet.contains(PresentAllDurations),
        !configSet.contains(PresentWithoutColor),
        configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
        cssUrl, 
        resultHolder
      )
      else
        new FilterReporter(
          new HtmlReporter(
            directory,
            configSet.contains(PresentAllDurations),
            !configSet.contains(PresentWithoutColor),
            configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
            configSet.contains(PresentFullStackTraces), // If they say both S and F, F overrules
            cssUrl, 
            resultHolder
          ),
          configSet
        )
  }
  
  protected def createCustomReporter(configSet: Set[ReporterConfigParam], reporterClassName: String, loader: ClassLoader) = {
    val customReporter = getCustomReporter(reporterClassName, loader, "-r... " + reporterClassName)
    if (configSet.isEmpty)
      customReporter
    else
      new FilterReporter(customReporter, configSet)
  }
  
  protected def createJunitXmlReporter(configSet: Set[ReporterConfigParam], directory: String) = {
    new JUnitXmlReporter(directory)
  }
  
  protected def createDashboardReporter(configSet: Set[ReporterConfigParam], directory: String, numFilesToArchive: Int) = {
    new DashboardReporter(directory, numFilesToArchive)
  }
  
  protected def createXmlSocketReporter(host: String, port: Int) = {
    new XmlSocketReporter(host, port)
  }
  
  protected def createSocketReporter(host: String, port: Int) = {
    new SocketReporter(host, port)
  }
  
  private[scalatest] def getReporterFromConfiguration(configuration: ReporterConfiguration, loader: ClassLoader, resultHolder: Option[SuiteResultHolder]): Reporter =
    configuration match {
      case StandardOutReporterConfiguration(configSet) => createStandardOutReporter(configSet)
      case StandardErrReporterConfiguration(configSet) => createStandardErrReporter(configSet)
      case FileReporterConfiguration(configSet, fileName) => createFileReporter(configSet, fileName)
      case JunitXmlReporterConfiguration(configSet, directory) => createJunitXmlReporter(configSet, directory)
      case DashboardReporterConfiguration(configSet, directory, numFilesToArchive) => createDashboardReporter(configSet, directory, numFilesToArchive)
      case XmlReporterConfiguration(configSet, directory) => createXmlReporter(configSet, directory)
      case HtmlReporterConfiguration(configSet, directory, cssFile) => createHtmlReporter(configSet, directory, cssFile, resultHolder)
      case CustomReporterConfiguration(configSet, reporterClassName) => createCustomReporter(configSet, reporterClassName, loader) 
      case GraphicReporterConfiguration(configSet) => throw new RuntimeException("Should never happen.")
      case SocketReporterConfiguration(host, port) => createSocketReporter(host, port)
      case XmlSocketReporterConfiguration(host, port) => createXmlSocketReporter(host, port)
  }
  
  private[scalatest] def createReportersFromConfigurations(reporterSpecs: ReporterConfigurations, loader: ClassLoader, resultHolder: Option[SuiteResultHolder]) = 
    (for (spec <- reporterSpecs)
        yield getReporterFromConfiguration(spec, loader, resultHolder))
  
  private[scalatest] def getDispatchReporter(reporterSpecs: ReporterConfigurations, graphicReporter: Option[Reporter], passFailReporter: Option[Reporter], loader: ClassLoader, resultHolder: Option[SuiteResultHolder]): DispatchReporter = {
    val reporterSeq = createReportersFromConfigurations(reporterSpecs, loader, resultHolder)
    getDispatchReporter(reporterSeq, graphicReporter, passFailReporter, loader, resultHolder)
  }
  
  private[scalatest] def getDispatchReporter(reporterSeq: Seq[Reporter], graphicReporter: Option[Reporter], passFailReporter: Option[Reporter], loader: ClassLoader, resultHolder: Option[SuiteResultHolder]): DispatchReporter = {
    val almostFullReporterList: List[Reporter] =
      graphicReporter match {
        case None => reporterSeq.toList
        case Some(gRep) => gRep :: reporterSeq.toList
      }
      
    val fullReporterList: List[Reporter] =
      passFailReporter match {
        case Some(pfr) => pfr :: almostFullReporterList
        case None => almostFullReporterList
      }

    new DispatchReporter(fullReporterList)
  }
}

private[scalatest] object ReporterFactory extends ReporterFactory
