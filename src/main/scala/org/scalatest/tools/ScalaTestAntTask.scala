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
package org.scalatest.tools

import scala.collection.mutable.ListBuffer
import org.apache.tools.ant.BuildException
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.Path
import org.apache.tools.ant.AntClassLoader
import org.apache.tools.ant.taskdefs.Java

/**
 * <p>
 * An ant task to run ScalaTest. Instructions on how to specify various
 * options are below.  See the main documentation for object <a href="Runner$.html"><code>Runner</code></a> class for a description
 * of what each of the options does.
 * </p>
 *
 * <p>
 * To use the ScalaTest ant task, you must first define it in your ant file using <code>taskdef</code>.
 * Here's an example:
 * </p>
 *
 * <pre>
 *  &lt;path id="scalatest.classpath"&gt;
 *    &lt;pathelement location="${lib}/scalatest.jar"/&gt;
 *    &lt;pathelement location="${lib}/scala-library.jar"/&gt;
 *    &lt;-- scala-actors.jar needed only for ScalaTest <= 1.9.1 on Scala >= 2.10.0 --&gt;
 *    &lt;pathelement location="${lib}/scala-actors.jar"/&gt;
 *  &lt;/path&gt;
 *
 *  &lt;target name="main" depends="dist"&gt;
 *    &lt;taskdef name="scalatest" classname="org.scalatest.tools.ScalaTestAntTask"&gt;
 *      &lt;classpath refid="scalatest.classpath"/&gt;
 *    &lt;/taskdef&gt;
 *
 *    &lt;scalatest ...
 *  &lt;/target&gt;
 * </pre>
 *
 * <p>
 * Note that you only need the <code>scala-actors.jar</code> if you are using ScalaTest version 1.9.1 or earlier
 * with Scala 2.10 or later.
 * Once defined, you use the task by specifying information in a <code>scalatest</code> element:
 * </p>
 *
 * <pre>
 *   &lt;scalatest ...&gt;
 *     ...
 *   &lt;/scalatest&gt;
 * </pre>
 *
 * <p>
 * You can place key value pairs into the <a href="Runner$.html#configMapSection">config map</a> using nested <code>&lt;config&gt;</code> elements,
 * like this:
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;config name="dbname" value="testdb"/&gt;
 *     &lt;config name="server" value="192.168.1.188"/&gt;
 * </pre>
 *
 * <p>
 * You can specify a <a href="Runner$.html#specifyingARunpath">runpath</a> using either a <code>runpath</code> attribute and/or nested
 * <code>&lt;runpath&gt;</code> elements, using standard ant path notation:
 * </p>
 *
 * <pre>
 *   &lt;scalatest runpath="serviceuitest-1.1beta4.jar:myjini"&gt;
 * </pre>
 *
 * <p>
 * or
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;runpath&gt;
 *       &lt;pathelement location="serviceuitest-1.1beta4.jar"/&gt;
 *       &lt;pathelement location="myjini"/&gt;
 *     &lt;/runpath&gt;
 * </pre>
 *
 * <p>
 * To add a URL to your runpath, use a <code>&lt;runpathurl&gt;</code> element
 * (since ant paths don't support URLs):
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;runpathurl url="http://foo.com/bar.jar"/&gt;
 * </pre>
 *
 * <p>
 * You can specify <a href="Runner$.html#specifyingReporters">reporters</a> using nested <code>&lt;reporter&gt;</code> elements, where the <code>type</code>
 * attribute must be one of the following:
 * </p>
 *
 * <ul>
 *   <li>  <code>graphic</code>          </li>
 *   <li>  <code>file</code>             </li>
 *   <li>  <code>memory</code>           </li>
 *   <li>  <code>junitxml</code>         </li>
 *   <li>  <code>html</code>             </li>
 *   <li>  <code>stdout</code>           </li>
 *   <li>  <code>stderr</code>           </li>
 *   <li>  <code>reporterclass</code>    </li>
 * </ul>
 *
 * <p>
 * Each may include a <code>config</code> attribute to specify the <a href="Runner$.html#configuringReporters">reporter configuration</a>.
 * Types <code>file</code>, <code>memory</code>, <code>junitxml</code>, <code>html</code>, and <code>reporterclass</code>
 * require additional attributes (the css attribute is optional for the html reporter):
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;reporter type="stdout" config="FD"/&gt;
 *     &lt;reporter type="file" filename="test.out"/&gt;
 *     &lt;reporter type="memory" filename="target/memory.out"/&gt;
 *     &lt;reporter type="junitxml" directory="target"/&gt;
 *     &lt;reporter type="html" directory="target" css="src/main/html/mystylesheet.css"/&gt;
 *     &lt;reporter type="reporterclass" classname="my.ReporterClass"/&gt;
 * </pre>
 *
 * <p>
 * Specify <a href="Runner$.html#specifyingTagsToIncludeAndExclude">tags to include and/or exclude</a> using <code>&lt;tagsToInclude&gt;</code> and
 * <code>&lt;tagsToExclude&gt;</code> elements, like this:
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;tagsToInclude&gt;
 *         CheckinTests
 *         FunctionalTests
 *     &lt;/tagsToInclude&gt;
 *
 *     &lt;tagsToExclude&gt;
 *         SlowTests
 *         NetworkTests
 *     &lt;/tagsToExclude&gt;
 * </pre>
 *
 * <p>
 * Tags to include or exclude can also be specified using attributes
 * tagsToInclude and tagsToExclude, with arguments specified as whitespace-
 * delimited lists.
 * </p>
 *
 * <p>
 * To specify <a href="Runner$.html#selectingSuitesAndTests">suites to run</a>, use either a <code>suite</code> attribute or nested
 * <code>&lt;suite&gt;</code> elements:
 * </p>
 *
 * <pre>
 *   &lt;scalatest suite="com.artima.serviceuitest.ServiceUITestkit"&gt;
 * </pre>
 *
 * <p>
 * or
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;suite classname="com.artima.serviceuitest.ServiceUITestkit"/&gt;
 * </pre>
 *
 * <p>
 * To specify <a href="Runner$.html#selectingSuitesAndTests">tests to run</a>, use nested <code>&lt;test&gt;</code> elements with
 * either a 'name' or 'substring' attribute:
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;test name="hello test"/&gt;
 *     &lt;test substring="hello"/&gt;
 * </pre>
 *
 * <p>
 * To specify suites using <a href="Runner$.html#membersOnlyWildcard">members-only or wildcard</a> package names, use
 * either the <code>membersonly</code> or <code>wildcard</code> attributes, or nested
 * <code>&lt;membersonly&gt;</code> or <code>&lt;wildcard&gt;</code> elements:
 * </p>
 *
 * <pre>
 *   &lt;scalatest membersonly="com.artima.serviceuitest"&gt;
 * </pre>
 *
 * <p>
 * or
 * </p>
 *
 * <pre>
 *   &lt;scalatest wildcard="com.artima.joker"&gt;
 * </pre>
 *
 * <p>
 * or
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;membersonly package="com.artima.serviceuitest"/&gt;
 *     &lt;wildcard package="com.artima.joker"/&gt;
 * </pre>
 *
 * <p>
 * Use attribute <code>suffixes="[pipe-delimited list of suffixes]"</code>
 * to specify that only classes whose names end in one of the specified <a href="Runner$.html#specifyingSuffixesToDiscover">suffixes</a>
 * should be included in discovery searches for Suites to test.  This can
 * be used to improve discovery time or to limit the scope of a test. E.g.:
 * </p>
 *
 * <pre>
 *   &lt;scalatest suffixes="Spec|Suite"&gt;
 * </pre>
 *
 * <p>
 * Use attribute <code>testsfile="[file name]"</code> or nested
 * <testsfile> elements to specify files containing a list of
 * tests to be run.  This is used to rerun failed/canceled tests
 * listed in files written by the memory reporter.  E.g.:
 * </p>
 *
 * <pre>
 *   &lt;scalatest testsfile="target/memory.out"&gt;
 * </pre>
 *
 * <p>
 * or
 * </p>
 *
 * <pre>
 *   &lt;scalatest&gt;
 *     &lt;testsfile filename="target/memory.out"/&gt;
 * </pre>
 *
 * <p>
 * Use attribute <code>parallel="true"</code> to specify <a href="Runner$.html#executingSuitesInParallel">parallel execution</a> of suites.
 * (If the <code>parallel</code> attribute is left out or set to false, suites will be executed sequentially by one thread.)
 * When <code>parallel</code> is true, you can include an optional <code>sortSuites</code> attribute to request that events be sorted on-the-fly so that
 * events for the same suite are reported together, with a timeout, (<em>e.g.</em>, <code>sortSuites="true"</code>),
 * and an optional <code>numthreads</code> attribute to specify the number
 * of threads to be created in thread pool (<em>e.g.</em>, <code>numthreads="10"</code>).
 * </p>
 *
 * <p>
 * Use attribute <code>haltonfailure="true"</code> to cause ant to fail the
 * build if there's a test failure.
 * </p>
 *
 * <p>
 * Use attribute <code>fork="true"</code> to cause ant to run the tests in
 * a separate process.
 * </p>
 *
 * <p>
 * When <code>fork</code> is <code>true</code>, attribute <code>maxmemory</code> may be used to specify
 * the maximum memory size that will be passed to the forked jvm.&nbsp; For example, the following setting
 * will cause <code>"-Xmx1280M"</code> to be passed to the java command used to
 * run the tests.
 * </p>
 *
 * <pre>
 *   &lt;scalatest maxmemory="1280M"&gt;
 * </pre>
 *
 * <p>
 * When <code>fork</code> is true, nested <code>&lt;jvmarg&gt;</code> elements may be used
 * to pass additional arguments to the forked jvm.
 * For example, if you are running into 'PermGen space' memory errors,
 * you could add the following <code>jvmarg</code> to bump up the JVM's <code>MaxPermSize</code> value:
 * </p>
 *
 * <pre>
 *   &lt;jvmarg value="-XX:MaxPermSize=128m"/&gt;
 * </pre>
 *
 * @author George Berger
 */
class ScalaTestAntTask extends Task {
  private var includes:  String = ""
  private var excludes:  String = ""
  private var maxMemory: String = null
  private var suffixes:  String = null

  private var parallel      = false
  private var sortSuites    = false
  private var haltonfailure = false
  private var fork          = false
  private var spanScaleFactor = 1.0

  private var numthreads = 0

  private val runpath      = new ListBuffer[String]
  private val jvmArgs      = new ListBuffer[String]
  private val suites       = new ListBuffer[SuiteElement]
  private val tests        = new ListBuffer[TestElement]
  private val membersonlys = new ListBuffer[String]
  private val wildcards    = new ListBuffer[String]
  private val testNGSuites = new ListBuffer[String]
  private val chosenStyles = new ListBuffer[String]
  private val testsfiles   = new ListBuffer[String]

  private val reporters  = new ListBuffer[ReporterElement]
  private val properties = new ListBuffer[NameValuePair]

  /**
   * Executes the task.
   */
  override def execute {
    val argsList = buildArgsList

    val success = if (fork) javaTaskRunner(argsList)
                  else      Runner.run(argsList.toArray)

    if (!success && haltonfailure)
      throw new BuildException("ScalaTest run failed.")
  }

  def buildArgsList: List[String] = {
    val args = new ListBuffer[String]

    addSuiteArgs(args)
    addTestArgs(args)
    addReporterArgs(args)
    addPropertyArgs(args)
    addIncludesArgs(args)
    addExcludesArgs(args)
    addRunpathArgs(args)
    addTestNGSuiteArgs(args)
    addParallelArg(args)
    addSuffixesArg(args)
    addTestsfileArgs(args)
    addChosenStyles(args)
    addSpanScaleFactorArg(args)

    args.toList
  }

  private def javaTaskRunner(args: List[String]): Boolean = {
    val java = new Java
    java.bindToOwner(this)
    java.init()
    java.setFork(true)
    java.setClassname("org.scalatest.tools.Runner")

    val classLoader = getClass.getClassLoader.asInstanceOf[AntClassLoader]

    java.setClasspath(new Path(getProject, classLoader.getClasspath))

    if (maxMemory != null) java.createJvmarg.setValue("-Xmx" + maxMemory)

    for (jvmArg <- jvmArgs)
      java.createJvmarg.setValue(jvmArg)

    for (arg <- args)
      java.createArg.setValue(arg)

    val result = java.executeJava

    return (result == 0)
  }

  //
  // Adds '-P runpath' arg pair to args list if a runpath
  // element or attribute was specified for task.
  //
  private def addRunpathArgs(args: ListBuffer[String]) {
    if (runpath.size > 0) {
      args += "-R"
      args += getSpacedOutPathStr(runpath.toList)
    }
  }

  private def addTestNGSuiteArgs(args: ListBuffer[String]) {
    if (testNGSuites.size > 0) {
      args += "-b"
      args += getSpacedOutPathStr(testNGSuites.toList)
    }
  }
  
  private def addChosenStyles(args: ListBuffer[String]) {
    chosenStyles.foreach { style => 
      args += "-y"
      args += style
    }
  }

  //
  // Adds '-C' arg to args list if 'parallel' attribute was
  // specified true for task.
  //
  private def addParallelArg(args: ListBuffer[String]) {
    if (parallel) {
      args += (if (sortSuites) "-PS" else "-P") + (if (numthreads > 0) ("" + numthreads) else "")
    }
  }

  //
  // Add -F arg to args list if spanScaleFactor attribute was 
  // specified for task
  //
  private def addSpanScaleFactorArg(args: ListBuffer[String]) {
    args += "-F"
    args += spanScaleFactor.toString
  }

  //
  // Adds '-q' arg to args list if 'suffixes' attribute was
  // specified for task.
  //
  private def addSuffixesArg(args: ListBuffer[String]) {
    if (suffixes != null) {
      args += "-q"
      args += suffixes
    }
  }

  //
  // Adds '-A' arg to args list if 'testsfile' attribute was
  // specified for task.
  //
  private def addTestsfileArgs(args: ListBuffer[String]) {
    for (testsfile <- testsfiles) {
      args += "-A"
      args += testsfile
    }
  }

  //
  // Adds '-n includes-list' arg pair to args list if a tagsToInclude
  // element or attribute was supplied for task.
  //
  private def addIncludesArgs(args: ListBuffer[String]) {
    if ((includes != null) && (includes.trim != "")) {
      args += "-n"
      args += singleSpace(includes)
    }
  }

  //
  // Adds '-l excludes-list' arg pair to args list if a tagsToExclude
  // element or attribute was supplied for task.
  //
  private def addExcludesArgs(args: ListBuffer[String]) {
    if ((excludes != null) && (excludes.trim != "")) {
      args += "-l"
      args += singleSpace(excludes)
    }
  }

  //
  // Adds '-Dname=value' argument to args list for each nested
  // <property> element supplied for task.
  //
  private def addPropertyArgs(args: ListBuffer[String]) {
    for (pair <- properties)
      args += "-D" + pair.getName + "=" + pair.getValue
  }

  //
  // Adds '-s classname' argument to args list for each suite
  // specified for task.  Adds '-m packagename' for each
  // membersonly element specified, and '-w packagename' for
  // each wildcard element specified.
  //
  private def addSuiteArgs(args: ListBuffer[String]) {
    for (suite <- suites) {
      if (suite == null)
        throw new BuildException(
          "missing classname attribute for <suite> element")
      args += "-s"
      args += suite.getClassName
      suite.getTestNames.foreach { tn => 
        if (tn == null)
          throw new BuildException("missing name attribute for <test> element")
        args += "-t"
        args += tn
      }
      suite.getNestedSuites.foreach { ns => 
        if (ns.getSuiteId == null)
          throw new BuildException("missing suiteId attribute for <nested> element")
        args += "-i"
        args += ns.getSuiteId
        ns.getTestNames.foreach { tn => 
          if (tn == null)
            throw new BuildException("missing name attribute for <test> element")
          args += "-t"
          args += tn
        }
      }
    }

    for (packageName <- membersonlys) {
      if (packageName == null)
        throw new BuildException(
          "missing package attribute for <membersonly> element")
      args += "-m"
      args += packageName
    }

    for (packageName <- wildcards) {
      if (packageName == null)
        throw new BuildException(
          "missing package attribute for <wildcard> element")
      args += "-w"
      args += packageName
    }
  }

  //
  // Adds '-t testname' or '-z substring' argument to args list for
  // each test specified for task.
  //
  private def addTestArgs(args: ListBuffer[String]) {
    for (test <- tests) {
      if (test.getName != null) {
        args += "-t"
        args += test.getName
      }
      else if (test.getSubstring != null) {
        args += "-z"
        args += test.getSubstring
      }
      else
        throw new BuildException(
          "missing name or substring attribute for <test> element")
    }
  }

  //
  // Adds appropriate reporter options to args list for each
  // nested reporter element specified for task.  Defaults to
  // stdout if no reporter specified.
  //
  private def addReporterArgs(args: ListBuffer[String]) {
    if (reporters.size == 0)
      args += "-o"

    for (reporter <- reporters) {
      reporter.getType match {
        case "stdout"        => addReporterOption(args, reporter, "-o")
        case "stderr"        => addReporterOption(args, reporter, "-e")
        case "graphic"       => addReporterOption(args, reporter, "-g")
        case "file"          => addFileReporter(args, reporter)
        case "memory"        => addMemoryReporter(args, reporter)
        case "xml"           => addXmlReporter(args, reporter)
        case "junitxml"      => addJunitXmlReporter(args, reporter)
        case "dashboard"     => addDashboardReporter(args, reporter)
        case "html"          => addHtmlReporter(args, reporter)
        case "reporterclass" => addReporterClass(args, reporter)

        case t =>
          throw new BuildException("unexpected reporter type [" + t + "]")
      }
    }
  }

  //
  // Adds specified option to args for reporter.  Appends reporter
  // config string to option if specified, e.g. "-eFD".
  //
  private def addReporterOption(args: ListBuffer[String],
                                reporter: ReporterElement,
                                option: String)
  {
    val config = reporter.getConfig

    if (config == null) args += option
    else                args += option + config
  }

  //
  // Adds '-f' file reporter option to args.  Appends reporter
  // config string to option if specified.  Adds reporter's
  // filename as additional argument, e.g. "-fFD", "filename".
  //
  private def addFileReporter(args: ListBuffer[String],
                              reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-f")

    if (reporter.getFilename == null)
      throw new BuildException(
        "reporter type 'file' requires 'filename' attribute")

    args += reporter.getFilename
  }

  //
  // Adds '-M' memory reporter option to args.  Adds reporter's
  // filename as additional argument, e.g. "-M", "filename".
  //
  private def addMemoryReporter(args: ListBuffer[String],
                                reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-M")

    if (reporter.getFilename == null)
      throw new BuildException(
        "reporter type 'memory' requires 'filename' attribute")

    args += reporter.getFilename
  }

  //
  // Adds '-x' xml reporter option to args.  Adds reporter's
  // directory as additional argument, e.g. "-x", "directory".
  // [disabled for now]
  //
  private def addXmlReporter(args: ListBuffer[String],
                             reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-x")

    if (reporter.getDirectory == null)
      throw new BuildException(
        "reporter type 'xml' requires 'directory' attribute")

    args += reporter.getDirectory
  }

  //
  // Adds '-u' junit xml reporter option to args.  Adds reporter's
  // directory as additional argument, e.g. "-u", "directory".
  //
  private def addJunitXmlReporter(args: ListBuffer[String],
                                  reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-u")

    if (reporter.getDirectory == null)
      throw new BuildException(
        "reporter type 'junitxml' requires 'directory' attribute")

    args += reporter.getDirectory
  }

  //
  // Adds '-d' Dashboard reporter option to args.  Adds reporter's
  // directory as additional argument, e.g. "-d", "directory".
  //
  private def addDashboardReporter(args: ListBuffer[String],
                                   reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-d")

    if (reporter.getDirectory == null)
      throw new BuildException(
        "reporter type 'dashboard' requires 'directory' attribute")

    args += reporter.getDirectory

    if (reporter.getNumfiles >= 0) {
      args += "-a"
      args += reporter.getNumfiles.toString
    }
  }

  //
  // Adds '-h' html reporter option to args.  Appends reporter
  // config string to option if specified.  Adds reporter's
  // filename as additional argument, e.g. "-hFD", "filename".
  //
  private def addHtmlReporter(args: ListBuffer[String],
                              reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-h")

    if (reporter.getDirectory == null)
      throw new BuildException(
        "reporter type 'html' requires 'directory' attribute")

    args += reporter.getDirectory
    
    if (reporter.getCss != null) {
      args += "-Y"
      args += reporter.getCss
    }
  }

  //
  // Adds '-C' reporter class option to args.  Appends
  // reporter config string to option if specified.  Adds
  // reporter's classname as additional argument, e.g. "-C",
  // "my.ReporterClass".
  //
  private def addReporterClass(args: ListBuffer[String],
                               reporter: ReporterElement)
  {
    addReporterOption(args, reporter, "-C")

    if (reporter.getClassName == null)
      throw new BuildException(
        "reporter type 'reporterclass' requires 'classname' attribute")

    args += reporter.getClassName
  }

  /**
   * Sets value of the <code>runpath</code> attribute.
   */
  def setRunpath(runpath: Path) {
    for (element <- runpath.list) {
      this.runpath += element
    }
  }
  
  /**
   * Sets value of the <code>tagsToExclude</code> attribute.
   */
  def setTagsToExclude(tagsToExclude: String) {
    this.excludes += " " + tagsToExclude
  }
  
  /**
   * Sets value of the <code>tagsToInclude</code> attribute.
   */
  def setTagsToInclude(tagsToInclude: String) {
    this.includes += " " + tagsToInclude
  }
  
  /**
   * Sets value of the <code>haltonfailure</code> attribute.
   */
  def setHaltonfailure(haltonfailure: Boolean) {
    this.haltonfailure = haltonfailure
  }
  
  /**
   * Sets value of the <code>fork</code> attribute.
   */
  def setFork(fork: Boolean) {
    this.fork = fork
  }
  
  /**
   * Sets value of the <code>suffixes</code> attribute.
   */
  def setSuffixes(suffixes: String) {
    this.suffixes = suffixes
  }
  
  /**
   * Sets value of the <code>testsfile</code> attribute.
   */
  def setTestsfile(testsfile: String) {
    this.testsfiles += testsfile
  }
  
  /**
   * Sets value of the <code>maxmemory</code> attribute.
   */
  def setMaxmemory(max: String) {
    this.maxMemory = max
  }
  
  /**
   * Sets value of the <code>testngsuites</code> attribute.
   */
  def setTestNGSuites(testNGSuitePath: Path) {
    for (element <- testNGSuitePath.list)
      this.testNGSuites += element
  }

  /**
   * Sets value of the <code>concurrent</code> attribute.
   * <b>Note: The <code>concurrent</code> attribute has been deprecated and will be removed in a future version of ScalaTest.
   * Please use the <code>parallel</code> attribute instead.</b>
   */
  @deprecated("Please use parallel instead")
  def setConcurrent(concurrent: Boolean) {
    Console.err.println("WARNING: 'concurrent' attribute is deprecated " +
                        "- please use 'parallel' instead")
    this.parallel = concurrent
  }

  /**
   * Sets value of the <code>numthreads</code> attribute.
   */
  def setNumthreads(numthreads: Int) {
      this.numthreads = numthreads
  }

  /**
   * Sets value of the <code>parallel</code> attribute.
   */
  def setParallel(parallel: Boolean) {
      this.parallel = parallel
  }
  
  /**
   * Sets value of the <code>sortSuites</code> attribute.
   */
  def setSortSuites(sortSuites: Boolean) {
    this.sortSuites = sortSuites
  }
  
  /**
   * Sets value of the <code>spanScaleFactor</code> attribute.
   */
  def setSpanScaleFactor(spanScaleFactor: Double) {
    this.spanScaleFactor = spanScaleFactor
  }

  /**
   * Sets value from nested element <code>runpath</code>.
   */
  def addConfiguredRunpath(runpath: Path) {
    for (element <- runpath.list)
      this.runpath += element
  }
 
  /**
   * Sets value from nested element <code>testngsuites</code>.
   */
  def addConfiguredTestNGSuites(testNGSuitePath: Path) {
    for (element <- testNGSuitePath.list)
      this.testNGSuites += element
  }

  /**
   * Sets value from nested element <code>runpathurl</code>.
   */
  def addConfiguredRunpathUrl(runpathurl: RunpathUrl) {
    runpath += runpathurl.getUrl
  }

  /**
   * Sets value from nested element <code>jvmarg</code>.
   */
  def addConfiguredJvmArg(arg: JvmArg) {
    jvmArgs += arg.getValue
  }

  /**
   * Sets values from nested element <code>property</code>.
   * <b>The <code>property</code> attribute has been deprecated and will be removed in a future version of ScalaTest.
   * Please use the <code>config</code> attribute instead.</b>
   */
  @deprecated("Please use config instead")
  def addConfiguredProperty(property: NameValuePair) {
    Console.err.println("WARNING: <property> is deprecated - " +
                        "please use <config> instead [name: " +
                        property.getName + "]")
    properties += property
  }

  /**
   * Sets values from nested element <code>config</code>.
   */
  def addConfiguredConfig(config: NameValuePair) {
    properties += config
  }

  /**
   * Sets value of <code>suite</code> attribute.
   */
  def setSuite(suite: SuiteElement) {
    suites += suite
  }

  /**
   * Sets value of <code>membersonly</code> attribute.
   */
  def setMembersonly(packageName: String) {
    membersonlys += packageName
  }

  /**
   * Sets value of <code>wildcard</code> attribute.
   */
  def setWildcard(packageName: String) {
    wildcards += packageName
  }
  
  /**
   * Sets value of <code>style</code> attribute.
   */
  def setStyle(style: String) {
    chosenStyles += style
  }

  /**
   * Sets value from nested element <code>suite</code>.
   */
  def addConfiguredSuite(suite: SuiteElement) {
    suites += suite
  }

  /**
   * Sets value from nested element <code>test</code>.
   */
  def addConfiguredTest(test: TestElement) {
    tests += test
  }

  /**
   * Sets value from nested element <code>membersonly</code>.
   */
  def addConfiguredMembersOnly(membersonly: PackageElement) {
    membersonlys += membersonly.getPackage
  }

  /**
   * Sets value from nested element <code>wildcard</code>.
   */
  def addConfiguredWildcard(wildcard: PackageElement) {
    wildcards += wildcard.getPackage
  }

  /**
   * Sets value from nested element <code>reporter</code>.
   */
  def addConfiguredReporter(reporter: ReporterElement) {
    reporters += reporter
  }

  /**
   * Sets value from nested element <code>tagsToInclude</code>.
   */
  def addConfiguredTagsToInclude(tagsToInclude: TextElement) {
    this.includes += " " + tagsToInclude.getText
  }
  
  def addConfiguredStyle(style: StyleElement) {
    this.chosenStyles += style.getName
  }

  /**
   * Sets value from nested element <code>testsfile</code>.
   */
  def addConfiguredTestsfile(testsfile: TestsfileElement) {
    this.testsfiles += testsfile.getFilename
  }

  /**
   * Sets value from nested element <code>includes</code>.
   * <b>The <code>includes</code> attribute has been deprecated and will be removed in a future version of ScalaTest.
   * Please use the <code>tagsToInclude</code> attribute instead.</b>
   */
  @deprecated("Please use tagsToInclude instead")
  def addConfiguredIncludes(includes: TextElement) {
    Console.err.println("WARNING: 'includes' is deprecated - " +
                        "use 'tagsToInclude' instead [includes: " +
                        includes.getText + "]")
    addConfiguredTagsToInclude(includes)
  }

  /**
   * Sets value from nested element <code>tagsToExclude</code>.
   */
  def addConfiguredTagsToExclude(tagsToExclude: TextElement) {
    this.excludes += " " + tagsToExclude.getText
  }

  /**
   * Sets value from nested element <code>excludes</code>.
   * <b>The <code>excludes</code> attribute has been deprecated and will be removed in a future version of ScalaTest.
   * Please use the <code>tagsToExclude</code> attribute instead.</b>
   */
  @deprecated("Please use tagsToExclude instead")
  def addConfiguredExcludes(excludes: TextElement) {
    Console.err.println("WARNING: 'excludes' is deprecated - " +
                        "use 'tagsToExclude' instead [excludes: " +
                        excludes.getText + "]")
    addConfiguredTagsToExclude(excludes)
  }

  //
  // Translates a list of strings making up a path into a
  // single space-delimited string.  Uses backslashes to escape
  // spaces within individual path elements, since that's what
  // Runner's -p option expects.
  //
  private def getSpacedOutPathStr(path: List[String]): String = {
    path.map(_.replaceAll(" ", """\\ """)).mkString("", " ", "")
  }

  //
  // Translates a whitespace-delimited string into a
  // whitespace-delimited string, but not the same whitespace.  Trims
  // off leading and trailing whitespace and converts inter-element
  // whitespace to a single space.
  //
  private def singleSpace(str: String): String = {
    str.trim.replaceAll("\\s+", " ")
  }
}

  //
  // Class to hold data from <style> elements.
  //
  private class StyleElement {
    private var name: String = null
    
    def setName(name: String) {
      this.name = name
    }
    
    def getName = name
  }

  //
  // Class to hold data from <testsfile> elements.
  //
  private class TestsfileElement {
    private var filename: String = null
    
    def setFilename(filename: String) {
      this.filename = filename
    }
    
    def getFilename = filename
  }

  //
  // Class to hold data from <membersonly> and <wildcard> elements.
  //
  private class PackageElement {
    private var packageName: String = null

    def setPackage(packageName: String) {
      this.packageName = packageName
    }

    def getPackage = packageName
  }

  //
  // Class to hold data from <suite> elements.
  //
  private class SuiteElement {
    private var className: String = null
    private val testNamesBuffer = new ListBuffer[String]()
    private val nestedSuitesBuffer = new ListBuffer[NestedSuiteElement]()
    
    def setClassName(className: String) {
      this.className = className
    }

    def addConfiguredTest(test: TestElement) {
      testNamesBuffer += test.getName
    }
    
    def addConfiguredNested(nestedSuite: NestedSuiteElement) {
      nestedSuitesBuffer += nestedSuite
    }
    
    def getClassName = className
    def getTestNames = testNamesBuffer.toArray
    def getNestedSuites = nestedSuitesBuffer.toArray
  }
  
  private[tools] class TestElement {
    private var name: String = null
    private var substring: String = null
    
    def setName(name: String) {
      this.name = name
    }
    
    def setSubstring(substring: String) {
      this.substring = substring
    }
    
    def getName = name
    def getSubstring = substring
  }
  
  private class NestedSuiteElement {
    private var suiteId: String = null
    private val testNamesBuffer = new ListBuffer[String]()
    
    def setSuiteId(suiteId: String) {
      this.suiteId = suiteId
    }
    
    def addConfiguredTest(test: TestElement) {
      testNamesBuffer += test.getName
    }
    
    def getSuiteId = suiteId
    def getTestNames = testNamesBuffer.toArray
  }

  //
  // Class to hold data from tagsToInclude and tagsToExclude elements.
  //
  private class TextElement {
      private var text: String = null

      def addText(text: String) {
        this.text = text
      }

      def getText = text
  }

  //
  // Class to hold data from <property> elements.
  //
  private class NameValuePair {
    private var name  : String = null
    private var value : String = null

    def setName(name   : String) { this.name  = name  }
    def setValue(value : String) { this.value = value }

    def getName  = name
    def getValue = value
  }

  //
  // Class to hold data from <runpathurl> elements.
  //
  private class RunpathUrl {
    private var url: String = null

    def setUrl(url: String) { this.url = url }
    def getUrl = url
  }

  //
  // Class to hold data from <jvmarg> elements.
  //
  private class JvmArg {
    private var value: String = null

    def setValue(value: String) { this.value = value }
    def getValue = value
  }

  //
  // Class to hold data from <reporter> elements.
  //
  private class ReporterElement {
    private var rtype     : String = null
    private var config    : String = null
    private var filename  : String = null
    private var directory : String = null
    private var classname : String = null
    private var numfiles  : Int    = -1
    private var css       : String = null

    def setType(rtype          : String) { this.rtype     = rtype     }
    def setConfig(config       : String) { this.config    = config    }
    def setFilename(filename   : String) { this.filename  = filename  }
    def setDirectory(directory : String) { this.directory = directory }
    def setClassName(classname : String) { this.classname = classname }
    def setNumfiles(numfiles   : Int)    { this.numfiles  = numfiles  }
    def setCss(css: String) { this.css = css }

    def getType      = rtype
    def getConfig    = config
    def getFilename  = filename
    def getDirectory = directory
    def getClassName = classname
    def getNumfiles  = numfiles
    def getCss = css
  }

/*
 *   <li>  <code>dashboard</code>             </li>
 * Types <code>file</code>, <code>junitxml</code>, <code>dashboard</code>, and <code>reporterclass</code> require additional attributes
 *     &lt;reporter type="dashboard"          directory="target"/&gt;
 * <p>
 * For reporter type 'dashboard', an optional <code>numfiles</code> attribute may be
 * included to specify the number of old summary and duration files to be archived.
 * Default is 2.
 * </p>
 *
*/
