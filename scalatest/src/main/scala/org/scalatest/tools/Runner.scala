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

import org.scalatest._
import java.net.URL
import java.net.MalformedURLException
import java.net.URLClassLoader
import java.io.File
import java.io.IOException
import javax.swing.SwingUtilities
import java.util.concurrent.ArrayBlockingQueue
import java.util.regex.Pattern
import java.util.concurrent.Semaphore
import org.scalatest.events._
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.ThreadFactory
import SuiteDiscoveryHelper._
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.time.Millis
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.junit.JUnitWrapperSuite
import org.scalatest.testng.TestNGWrapperSuite
import Suite.{mergeMap, CHOSEN_STYLES, SELECTED_TAG, testSortingReporterTimeout}
import ArgsParser._
import org.scalactic.Requirements._

/*
Command line args:

a - archive count for dashboard reporter     --archive
A - select previously failed and/or canceled tests to rerun --again
b - run a testNG test (b for Beust)          --testng
B - 
c - parallel execution (--parallel)          --parallel (deprecated, will be custom reporter)
C - custom reporter (temporarily)
d - dashboard reporter                       --dashboard
D - config map pair                          -D
e - standard error reporter (--stderr-reporter)    --stderr
E -
f - file reporter                            --file
F - Span scale factor
g - graphical reporter                       --graphical
G -
h - HTML Reporter                            --html
H -
i - this one is used for the Suite ID        --suiteId
I -
j - currently JUnit directly (can drop and use WrapWith)   --junit
J - 
k - socket reporter XML
K - socket reporter binary
l - tags to exclude                          --exclude
L -
m - members only path                        --members
M - memory reporter                          --memory (records failed tests in a file, so they can be rerun with A)
n - tags to include                          --include
N -
o - standard out reporter
O -
p - space-separated runpath (currently deprecated, will be parallel execution)
P - parallel execution (temporarily)
q - Suffix? -q Spec will only look at class files whose name ends in Spec
Q - equalivalent to -q Suite -q Spec
r - custom reporter (currently deprecated, will be runpath)
R - space-separated runpath (temporarily)
s - suite class name (to become a glob)
S -
t - test name
T - sorting timeout                        --sorting-timeout
u - JUnit XML reporter
U -
v - ScalaTest version number (also -version and --version)
V -
w - wildcard path
W - slowpoke detector (with two integral args, for timeout (delay) and interval (period), in seconds)
x - save for ScalaTest native XML
X -
y - sets org.scalatest.chosenstyle -y FunSpec or -y "FunSpec FunSuite"
-Y for after -h to set a custom style sheet
z - test name wildcard
Z -

StringReporter configuration params:
A - drop AlertProvided events
B - drop NoteProvided events
C - drop TestSucceeded events
D - show all durations
E - drop TestPending events
F - show full stack traces
*G - reminder with full stack traces
H - drop SuiteStarting events
*I - Reminder without stack traces
J
*K - exclude TestCanceled events from reminder
L - drop SuiteCompleted events
M - drop MarkupProvided events
N - drop TestStarting events
O - drop InfoProvided events
P - drop ScopeOpened events
Q - drop ScopeClosed events
R - drop ScopePending events 
S - show short stack traces
*T - reminder with short stack traces
U - unformatted mode
V
W - without color
X - drop TestIgnored events
Z
*/

private[tools] case class SuiteConfig(suite: Suite, dynaTags: DynaTags, requireSelectedTag: Boolean, excludeNestedSuites: Boolean)
private[scalatest] case class ConcurrentConfig(numThreads: Int, enableSuiteSortingReporter: Boolean)
private[tools] case class SlowpokeConfig(delayInMillis: Long, periodInMillis: Long)

/**
 * Application that runs a suite of tests.
 *
 * <p>
 * Note: this application offers the full range of ScalaTest features via command line arguments described below. If you just want
 * to run a suite of tests from the command line and see results on the standard output, you may prefer to use <a href="../run$.html">ScalaTest's simple runner</a>.
 * </p>
 *
 * <p>
 * The basic form of a <code>Runner</code> invocation is:
 * </p>
 *
 * <pre class="stExamples">
 * scala [-cp scalatest-&lt;version&gt;.jar:...] org.scalatest.tools.Runner [arguments]
 * </pre>
 *
 * <p>
 * The arguments <code>Runner</code> accepts are described in the following table:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">argument</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">description</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">example</th></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-D<em>key</em>=<em>value</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">defines a key/value pair for the <a href="#configMapSection"><em>config map</em></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-DmaxConnections=100</code></a></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-R <em>&lt;runpath elements&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">the <a href="#specifyingARunpath">specifies the <em>runpath</em></a> from which tests classes will be<br/>discovered and loaded (Note: only one <code>-R</code> allowed)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><em>Unix</em>: <code>-R target/classes:target/generated/classes</code><br/><em>Windows</em>: <code>-R target\classes;target\generated\classes</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-n <em>&lt;tag name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><a href="#specifyingTagsToIncludeAndExclude">specifies a tag to include</a> (Note: only one tag name allowed per <code>-n</code>)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-n UnitTests -n FastTests</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-l <em>&lt;tag name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><a href="#specifyingTagsToIncludeAndExclude">specifies a tag to exclude</a> (Note: only one tag name allowed per <code>-l</code>)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-l SlowTests -l PerfTests</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-P<em>[S][integer thread count]</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><a href="#executingSuitesInParallel">specifies a parallel run</a>, with optional suite sorting and thread count<br/>(Note: only one <code>-P</code> allowed)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-P</code>, <code>-PS</code>, <code>-PS 8</code>, <em>or</em> <code>-P8</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-s <em>&lt;suite class name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">specifies a <a href="#executingSuites">suite class</a> to run</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-s com.company.project.StackSpec</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-m <em>&lt;members-only package&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">requests that suites that are <a href="#membersOnlyWildcard">direct members of the specified package</a><br/> be discovered and run</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-m com.company.project</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-w <em>&lt;wildcard package&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">requests that suites that are <a href="#membersOnlyWildcard">members of the specified package or its subpackages</a><br/>be discovered and run</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-w com.company.project</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-q <em>&lt;suffixes&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">specify <a href="#specifyingSuffixesToDiscover">suffixes to discover</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-q Spec -q Suite</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-Q</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">discover only classes whose names end with <code>Spec</code> or <code>Suite</code><br/>(or other suffixes specified by <code>-q</code>)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-Q</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-j <em>&lt;JUnit class name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">instantiate and run a <a href="#specifyingJUnitTests">JUnit test class</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-j StackTestClass</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-b <em>&lt;TestNG XML file&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">run <a href="#specifyingTestNGXML">TestNG tests</a> using the specified TestNG XML file</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-b testng.xml</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-F <em>&lt;span scale factor&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">a factor by which to <a href="#scalingTimeSpans">scale time spans</a><br/>(Note: only one <code>-F</code> is allowed)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-F 10</code> <em>or</em> <code>-F 2.5</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-T <em>&lt;sorting timeout&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">specifies a integer timeout (in seconds) for sorting the events of<br/>parallel runs back into sequential order</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-T 5</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-y <em>&lt;chosen styles&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">specifies <a href="#specifyingChosenStyles">chosen styles</a> for your project</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-y org.scalatest.FlatSpec</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-i <em>&lt;suite ID&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">specifies a <a href="#selectingSuitesAndTests">suite to run by ID</a> (Note: must follow <code>-s</code>, <br/>and is intended to be used primarily by tools such as IDEs.)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-i com.company.project.FileSpec-file1.txt</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-t <em>&lt;test name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><a href="#selectingSuitesAndTests">select the test</a> with the specified name</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-t "An empty Stack should complain when popped"</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-z <em>&lt;test name substring&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><a href="#selectingSuitesAndTests">select tests</a> whose names include the specified substring</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-z "popped"</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-g<em>[NCXEHLOPQMD]</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the graphical reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-g</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-f<em>[NCXEHLOPQMDWSFU] &lt;filename&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the file reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-f output.txt</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-u <em>&lt;directory name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the JUnit XML reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-u target/junitxmldir</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-h <em>&lt;directory name&gt;</em> [-Y <em>&lt;css file name&gt;</em>]</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the HTML reporter, optionally including the specified CSS file</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-h target/htmldir -Y src/main/html/customStyles.css</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-v</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">print the ScalaTest version</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-v</code> <em>or, also</em> <code>-version</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-o<em>[NCXEHLOPQMDWSFU]</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the standard output reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-o</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-e<em>[NCXEHLOPQMDWSFU]</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select the standard error reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-e</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-C<em>[NCXEHLOPQMD] &lt;reporter class&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">select a custom reporter</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-C com.company.project.BarReporter</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-M <em>&lt;file name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">memorize failed and canceled tests in a file, so they can be rerun with -A (again)</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-M rerun.txt</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-A <em>&lt;file name&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">used in conjunction with -M (momento) to select previously failed<br/>and canceled tests to rerun again</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-A rerun.txt</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-W <em>&lt;delay&gt;</em> <em>&lt;period&gt;</em></code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">requests <a href="#slowpokeNotifications">notifications of <em>slowpoke</em> tests</a>, tests that have been running<br/>longer than <em>delay</em> seconds, every <em>period</em> seconds.</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>-W 60 60</code></td></tr>
 * </table>
 *
 * <p>
 * The simplest way to start <code>Runner</code> is to specify the directory containing your compiled tests as the sole element of the runpath, for example:
 * </p>
 *
 * <pre class="stExamples">scala -classpath scalatest-&lt;version&gt;.jar org.scalatest.tools.Runner -R compiled_tests</pre>
 *
 * <p>
 * Given the previous command, <code>Runner</code> will discover and execute all <code>Suite</code>s in the <code>compiled_tests</code> directory and its subdirectories,
 * and show results in graphical user interface (GUI).
 * </p>
 *
 * <a name="executingSuites"></a>
 * <h2>Executing suites</h2>
 *
 * <p>
 * Each <code>-s</code> argument must be followed by one and only one fully qualified class name. The class must either extend <code>Suite</code> and
 * have a public, no-arg constructor, or be annotated by a valid <code>WrapWith</code> annotation.
 * </p>
 *
 * <a name="configMapSection"></a>
 * <h2>Specifying the config map</h2>
 *
 * <p>
 * A <em>config map</em> contains pairs consisting of a string key and a value that may be of any type. (Keys that start with
 * &quot;org.scalatest.&quot; are reserved for ScalaTest. Configuration values that are themselves strings may be specified on the
 * <code>Runner</code> command line.
 * Each configuration pair is denoted with a "-D", followed immediately by the key string, an &quot;=&quot;, and the value string.
 * For example:
 * </p>
 *
 * <pre class="stExamples">-Ddbname=testdb -Dserver=192.168.1.188</pre>
 *
 * <a name="specifyingARunpath"></a>
 * <h2>Specifying a runpath</h2>
 *
 * <p>
 * A runpath is the list of filenames, directory paths, and/or URLs that <code>Runner</code>
 * uses to load classes for the running test. If runpath is specified, <code>Runner</code> creates
 * a custom class loader to load classes available on the runpath.
 * The graphical user interface reloads the test classes anew for each run
 * by creating and using a new instance of the custom class loader for each run.
 * The classes that comprise the test may also be made available on
 * the classpath, in which case no runpath need be specified.
 * </p>
 *
 * <p>
 * The runpath is specified with the <b>-R</b> option. The <b>-R</b> must be followed by a space,
 * a double quote (<code>"</code>), a white-space-separated list of
 * paths and URLs, and a double quote. If specifying only one element in the runpath, you can leave off
 * the double quotes, which only serve to combine a white-space separated list of strings into one
 * command line argument. If you have path elements that themselves have a space in them, you must
 * place a backslash (\) in front of the space. Here's an example:
 * </p>
 *
 * <pre class="stExamples">-R "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar target/class\ files"</pre>
 *
 * <a name="specifyingReporters"></a>
 * <h2>Specifying reporters</h2>
 *
 * <p>
 * Reporters can be specified  on the command line in any of the following
 * ways:
 * </p>
 *
 * <ul>
 * <li> <code><b>-g[configs...]</b></code> - causes display of a graphical user interface that allows
 *    tests to be run and results to be investigated</li>
 * <li> <code><b>-f[configs...] &lt;filename&gt;</b></code> - causes test results to be written to
 *     the named file</li>
 * <li> <code><b>-u &lt;directory&gt;</b></code> - causes test results to be written to
 *      junit-style xml files in the named directory</li>
 * <li> <code><b>-h &lt;directory&gt; [-Y &lt;CSS file&gt;]</b></code> - causes test results to be written to
 *      HTML files in the named directory, optionally included the specified CSS file</li>
 * <li> <code><b>-a &lt;number of files to archive&gt;</b></code> - causes specified number of old
 *      summary and durations files to be archived (in summaries/ and durations/ subdirectories)
 *      for dashboard reporter (default is two)</li>
 * <li> <code><b>-o[configs...]</b></code> - causes test results to be written to
 *     the standard output</li>
 * <li> <code><b>-e[configs...]</b></code> - causes test results to be written to
 *     the standard error</li>
 * <li> <code><b>-k &lt;host&gt; &lt;port&gt;</b></code> - causes test results to be written to
 *      socket in the named host and port number, using XML format</li>
 * <li> <code><b>-K &lt;host&gt; &lt;port&gt;</b></code> - causes test results to be written to
 *      socket in the named host and port number, using Java object binary format</li>
 * <li> <code><b>-C[configs...] &lt;reporterclass&gt;</b></code> - causes test results to be reported to
 *     an instance of the specified fully qualified <code>Reporter</code> class name</li>
 * </ul>
 *
 * <p>
 * The <code><b>[configs...]</b></code> parameter, which is used to configure reporters, is described in the next section.
 * </p>
 *
 * <p>
 * The <code><b>-C</b></code> option causes the reporter specified in
 * <code><b>&lt;reporterclass&gt;</b></code> to be
 * instantiated.
 * Each reporter class specified with a <b>-C</b> option must be public, implement
 * <code>org.scalatest.Reporter</code>, and have a public no-arg constructor.
 * Reporter classes must be specified with fully qualified names. 
 * The specified reporter classes may be
 * deployed on the classpath. If a runpath is specified with the
 * <code>-R</code> option, the specified reporter classes may also be loaded from the runpath.
 * All specified reporter classes will be loaded and instantiated via their no-arg constructor.
 * </p>
 *
 * <p>
 * For example, to run a suite named <code>MySuite</code> from the <code>mydir</code> directory
 * using two reporters, the graphical reporter and a file reporter
 * writing to a file named <code>"test.out"</code>, you would type:
 * </p>
 *
 * <pre class="stExamples">java -jar scalatest.jar -R mydir <b>-g -f test.out</b> -s MySuite</pre>
 *
 * <p>
 * The <code><b>-g</b></code>, <code><b>-o</b></code>, or <code><b>-e</b></code> options can
 * appear at most once each in any single command line.
 * Multiple appearances of <code><b>-f</b></code> and <code><b>-C</b></code> result in multiple reporters
 * unless the specified <code><b>&lt;filename&gt;</b></code> or <code><b>&lt;reporterclass&gt;</b></code> is
 * repeated. If any of <code><b>-g</b></code>, <code><b>-o</b></code>, <code><b>-e</b></code>,
 * <code><b>&lt;filename&gt;</b></code> or <code><b>&lt;reporterclass&gt;</b></code> are repeated on
 * the command line, the <code>Runner</code> will print an error message and not run the tests.
 * </p>
 *
 * <p>
 * <code>Runner</code> adds the reporters specified on the command line to a <em>dispatch reporter</em>,
 * which will dispatch each method invocation to each contained reporter. <code>Runner</code> will pass
 * the dispatch reporter to executed suites. As a result, every
 * specified reporter will receive every report generated by the running suite of tests.
 * If no reporters are specified, a graphical
 * runner will be displayed that provides a graphical report of
 * executed suites.
 * </p>
 *
 * <a name="configuringReporters"></a>
 * <h2>Configuring reporters</h2>
 *
 * <p>
 * Each reporter option on the command line can include configuration characters. Configuration characters
 * are specified immediately following the <code><b>-g</b></code>, <code><b>-o</b></code>,
 * <code><b>-e</b></code>, <code><b>-f</b></code>, or <code><b>-C</b></code>. The following configuration
 * characters, which cause reports to be dropped, are valid for any reporter:
 * </p>
 *
 * <ul>
 * <li> <code><b>N</b></code> - drop <code>TestStarting</code> events</li>
 * <li> <code><b>C</b></code> - drop <code>TestSucceeded</code> events</li>
 * <li> <code><b>X</b></code> - drop <code>TestIgnored</code> events</li>
 * <li> <code><b>E</b></code> - drop <code>TestPending</code> events</li>
 * <li> <code><b>H</b></code> - drop <code>SuiteStarting</code> events</li>
 * <li> <code><b>L</b></code> - drop <code>SuiteCompleted</code> events</li>
 * <li> <code><b>O</b></code> - drop <code>InfoProvided</code> events</li>
 * <li> <code><b>P</b></code> - drop <code>ScopeOpened</code> events</li>
 * <li> <code><b>Q</b></code> - drop <code>ScopeClosed</code> events</li>
 * <li> <code><b>R</b></code> - drop <code>ScopePending</code> events</li>
 * <li> <code><b>M</b></code> - drop <code>MarkupProvided</code> events</li>
 * </ul>
 *
 * <p>
 * A dropped event will not be delivered to the reporter at all. So the reporter will not know about it and therefore not
 * present information about the event in its report. For example, if you specify <code>-oN</code>, the standard output reporter
 * will never receive any <code>TestStarting</code> events and will therefore never report them. The purpose of these
 * configuration parameters is to allow users to selectively remove events they find add clutter to the report without
 * providing essential information.
 * </p>
 *
 * <p>
 * The following three reporter configuration parameters may additionally be used on standard output (-o), standard error (-e),
 * and file (-f) reporters: 
 * </p>
 *
 * <ul>
 * <li> <code><b>W</b></code> - without color</li>
 * <li> <code><b>D</b></code> - show all durations</li>
 * <li> <code><b>S</b></code> - show short stack traces</li>
 * <li> <code><b>F</b></code> - show full stack traces</li>
 * <li> <code><b>U</b></code> - unformatted mode</li>
 * <li> <code><b>I</b></code> - show reminder of failed and canceled tests without stack traces</li>
 * <li> <code><b>T</b></code> - show reminder of failed and canceled tests with short stack traces</li>
 * <li> <code><b>G</b></code> - show reminder of failed and canceled tests with full stack traces</li>
 * <li> <code><b>K</b></code> - exclude <code>TestCanceled</code> events from reminder</li>
 * </ul>
 *
 * <p>
 * If you specify a W, D, S, F, U, R, T, G, or K for any reporter other than standard output, standard error, or file reporters, <code>Runner</code>
 * will complain with an error message and not perform the run.
 * </p>
 *
 * <p>
 * Configuring a standard output, error, or file reporter with <code>D</code> will cause that reporter to
 * print a duration for each test and suite.  When running in the default mode, a duration will only be printed for
 * the entire run.
 * </p>
 *
 * <p>
 * Configuring a standard output, error, or file reporter with <code>F</code> will cause that reporter to print full stack traces for all exceptions,
 * including <code>TestFailedExceptions</code>. Every <code>TestFailedException</code> contains a stack depth of the
 * line of test code that failed so that users won't need to search through a stack trace to find it. When running in the default,
 * mode, these reporters will only show full stack traces when other exceptions are thrown, such as an exception thrown
 * by production code. When a <code>TestFailedException</code> is thrown in default mode, only the source filename and
 * line number of the line of test code that caused the test to fail are printed along with the error message, not the full stack
 * trace. 
 * </p>
 *
 * <p>
 * The 'U' unformatted configuration removes some formatting from the output and adds verbosity.
 * The purpose of unformatted (or, "ugly") mode is to facilitate debugging of parallel runs. If you have
 * tests that fail or hang during parallel runs, but succeed when run sequentially, unformatted mode can help.
 * In unformatted mode, you can see exactly what is happening when it is happening. Rather than attempting to make the output
 * look as pretty and human-readable as possible, unformatted mode will just print out verbose information about each event
 * as it arrives, helping you track down the problem
 * you are trying to debug.
 * </p>
 *
 * <p>
 * By default, a standard output, error, or file reporter inserts ansi escape codes into the output printed to change and later reset
 * terminal colors. Information printed as a result of run starting, completed, and stopped events
 * is printed in cyan. Information printed as a result of ignored or pending test events is shown in yellow. Information printed
 * as a result of test failed, suite aborted, or run aborted events is printed in red. All other information is printed in green.
 * The purpose of these colors is to facilitate speedy reading of the output, especially the finding of failed tests, which can
 * get lost in a sea of passing tests. Configuring a standard output, error, or file reporter into without-color mode (<code>W</code>) will
 * turn off this behavior. No ansi codes will be inserted.
 * </p>
 *
 * <p>
 * The <code>R</code>, <code>T</code>, and <code>G</code> options enable "reminders" of failed and, optionally, canceled tests to be printed
 * at the end of the summary. This minimizes or eliminates the need to search and scroll backwards to find out what tests failed or were canceled.
 * For large test suites, the actual failure message could have scrolled off the top of the buffer, making it otherwise impossible
 * to see what failed. You can configure the detail level of the stack trace for regular reports of failed and canceled tests independently
 * from that of reminders. To set the detail level for regular reports, use <code>S</code> for short stack traces, <code>F</code> for
 * full stack traces, or nothing for the default of no stack trace. To set the detail level for reminder reports, use <code>T</code> for
 * reminders with short stack traces, <code>G</code> for reminders with full stack traces in reminders, or <code>R</code> for reminders
 * with no stack traces. If you wish to exclude reminders of canceled tests, <em>i.e.</em>, only see reminders of failed tests, specify
 * <code>K</code> along with one of <code>R</code>, <code>T</code>, or <code>G</code>, as in <code>"-oRK"</code>.
 * </p>
 *
 * <p>
 * For example, to run a suite using two reporters, the graphical reporter configured to present every reported event
 * and a standard error reporter configured to present everything but test starting, test succeeded, test ignored, test
 * pending, suite starting, suite completed, and info provided events, you would type:
 * </p>
 *
 * <p>
 * <code>scala -classpath scalatest-&lt;version&gt;.jar -R mydir <strong>-g -eNDXEHLO</strong> -s MySuite</code>
 * </p>
 *
 * <p>
 * Note that no white space is allowed between the reporter option and the initial configuration
 * parameters. So <code>"-e NDXEHLO"</code> will not work,
 * <code>"-eNDXEHLO"</code> will work.
 * </p>
 *
 * <a name="specifyingTagsToIncludeAndExclude"></a>
 * <h2>Specifying tags to include and exclude</h2>
 *
 * <p>
 * You can specify tag names of tests to include or exclude from a run. To specify tags to include,
 * use <code>-n</code> followed by a white-space-separated list of tag names to include, surrounded by
 * double quotes. (The double quotes are not needed if specifying just one tag.)  Similarly, to specify tags
 * to exclude, use <code>-l</code> followed by a white-space-separated
 * list of tag names to exclude, surrounded by double quotes. (As before, the double quotes are not needed
 * if specifying just one tag.) If tags to include is not specified, then all tests
 * except those mentioned in the tags to exclude (and in the <code>org.scalatest.Ignore</code> tag), will be executed.
 * (In other words, the absence of a <code>-n</code> option is like a wildcard, indicating all tests be included.)
 * If tags to include is specified, then only those tests whose tags are mentioned in the argument following <code>-n</code>
 * and not mentioned in the tags to exclude, will be executed. For more information on test tags, see
 * the <a href="../Suite.html">documentation for <code>Suite</code></a>. Here are some examples:
 * </p>
 *
 * <p>
 * <ul>
 * <li><code>-n CheckinTests</code></li>
 * <li><code>-n FunctionalTests -l org.scalatest.tags.Slow</code></li>
 * <li><code>-n "CheckinTests FunctionalTests" -l "org.scalatest.tags.Slow org.scalatest.tags.Network"</code></li>
 * </ul>
 * </p>
 *
 * <a name="specifyingSuffixesToDiscover"></a>
 * <h2>Specifying suffixes to discover</h2>
 *
 * <p>
 * You can specify suffixes of <code>Suite</code> names to discover. To specify suffixes to discover,
 * use <code>-q</code> followed by a vertical-bar-separated list of suffixes to discover, surrounded by
 * double quotes. (The double quotes are not needed if specifying just one suffix.)  Or you can specify
 * them individually using multiple -q's.
 * If suffixes to discover is not specified, then all suffixes are considered.
 * If suffixes is specified, then only those Suites whose class names end in one of the specified suffixes
 * will be considered during discovery. Here are some examples:
 * </p>
 *
 * <p>
 * <ul>
 * <li><code>-q Spec</code></li>
 * <li><code>-q "Spec|Suite"</code></li>
 * <li><code>-q Spec -q Suite</code></li>
 * </ul>
 * </p>
 *
 * <p>
 * Option -Q can be used to specify a default set of suffixes "Spec|Suite". If you specify both -Q and -q, you'll get Spec
 * and Suite in addition to the other suffix or suffixes you specify with -q.
 * </p>
 *
 * <p>
 * Specifying suffixes can speed up the discovery process because class files with names not ending the specified suffixes
 * can be immediately disqualified, without needing to load and inspect them to see if they either extend <code>Suite</code>
 * and declare a public, no-arg constructor, or are annotated with <code>WrapWith</code>. 
 * </p>
 *
 * <a name="executingSuitesInParallel"></a>
 * <h2>Executing <code>Suite</code>s in parallel</h2>
 *
 * <p>
 * With the proliferation of multi-core architectures, and the often parallelizable nature of tests, it is useful to be able to run
 * tests in parallel. If you include <code>-P</code> on the command line, <code>Runner</code> will pass a <code>Distributor</code> to
 * the <code>Suite</code>s you specify with <code>-s</code>. <code>Runner</code> will set up a thread pool to execute any <code>Suite</code>s
 * passed to the <code>Distributor</code>'s <code>put</code> method in parallel. Trait <code>Suite</code>'s implementation of
 * <code>runNestedSuites</code> will place any nested <code>Suite</code>s into this <code>Distributor</code>. Thus, if you have a <code>Suite</code>
 * of tests that must be executed sequentially, you should override <code>runNestedSuites</code> as described in the <a href="../Distributor.html">documentation for <code>Distributor</code></a>.
 * </p>
 *
 * <p>
 * The <code>-P</code> option may optionally be appended with a number (e.g.
 * "<code>-P10</code>" -- no intervening space) to specify the number of
 * threads to be created in the thread pool.  If no number (or 0) is
 * specified, the number of threads will be decided based on the number of
 * processors available.
 * </p>
 *
 * <a name="specifyingSuites"></a>
 * <h2>Specifying <code>Suite</code>s</h2>
 *
 * <p>
 * Suites are specified on the command line with a <b>-s</b> followed by the fully qualified
 * name of a <code>Suite</code> subclass, as in:
 * </p>
 *
 * <pre class="stExamples">-s com.artima.serviceuitest.ServiceUITestkit</pre>
 *
 * <p>
 * Each specified suite class must be public, a subclass of
 * <code>org.scalatest.Suite</code>, and contain a public no-arg constructor.
 * <code>Suite</code> classes must be specified with fully qualified names. 
 * The specified <code>Suite</code> classes may be
 * loaded from the classpath. If a runpath is specified with the
 * <code>-R</code> option, specified <code>Suite</code> classes may also be loaded from the runpath.
 * All specified <code>Suite</code> classes will be loaded and instantiated via their no-arg constructor.
 * </p>
 *
 * <p>
 * The runner will invoke <code>execute</code> on each instantiated <code>org.scalatest.Suite</code>,
 * passing in the dispatch reporter to each <code>execute</code> method.
 * </p>
 *
 * <p>
 * <code>Runner</code> is intended to be used from the command line. It is included in <code>org.scalatest</code>
 * package as a convenience for the user. If this package is incorporated into tools, such as IDEs, which take
 * over the role of runner, object <code>org.scalatest.tools.Runner</code> may be excluded from that implementation of the package.
 * All other public types declared in package <code>org.scalatest.tools.Runner</code> should be included in any such usage, however,
 * so client software can count on them being available.
 * </p>
 *
 * <a name="membersOnlyWildcard"></a>
 * <h2>Specifying "members-only" and "wildcard" <code>Suite</code> paths</h2>
 *
 * <p>
 * If you specify <code>Suite</code> path names with <code>-m</code> or <code>-w</code>, <code>Runner</code> will automatically
 * discover and execute accessible <code>Suite</code>s in the runpath that are either a member of (in the case of <code>-m</code>)
 * or enclosed by (in the case of <code>-w</code>) the specified path. As used in this context, a <em>path</em> is a portion of a fully qualified name.
 * For example, the fully qualifed name <code>com.example.webapp.MySuite</code> contains paths <code>com</code>, <code>com.example</code>, and <code>com.example.webapp</code>.
 * The fully qualifed name <code>com.example.webapp.MyObject.NestedSuite</code> contains paths <code>com</code>, <code>com.example</code>,
 * <code>com.example.webapp</code>, and <code>com.example.webapp.MyObject</code>.
 * An <em>accessible <code>Suite</code></em> is a public class that extends <code>org.scalatest.Suite</code>
 * and defines a public no-arg constructor. Note that <code>Suite</code>s defined inside classes and traits do not have no-arg constructors,
 * and therefore won't be discovered. <code>Suite</code>s defined inside singleton objects, however, do get a no-arg constructor by default, thus
 * they can be discovered.
 * </p>
 *
 * <p>
 * For example, if you specify <code>-m com.example.webapp</code>
 * on the command line, and you've placed <code>com.example.webapp.RedSuite</code> and <code>com.example.webapp.BlueSuite</code>
 * on the runpath, then <code>Runner</code> will instantiate and execute both of those <code>Suite</code>s. The difference
 * between <code>-m</code> and <code>-w</code> is that for <code>-m</code>, only <code>Suite</code>s that are direct members of the named path
 * will be discovered. For <code>-w</code>, any <code>Suite</code>s whose fully qualified
 * name begins with the specified path will be discovered. Thus, if <code>com.example.webapp.controllers.GreenSuite</code>
 * exists on the runpath, invoking <code>Runner</code> with <code>-w com.example.webapp</code> will cause <code>GreenSuite</code>
 * to be discovered, because its fully qualifed name begins with <code>"com.example.webapp"</code>. But if you invoke <code>Runner</code>
 * with <code>-m com.example.webapp</code>, <code>GreenSuite</code> will <em>not</em> be discovered because it is directly
 * a member of <code>com.example.webapp.controllers</code>, not <code>com.example.webapp</code>.
 * </p>
 *
 * <p>
 * If you specify no <code>-s</code>, <code>-m</code>, or <code>-w</code> arguments on the command line to <code>Runner</code>, it will discover and execute all accessible <code>Suite</code>s
 * in the runpath.
 * </p>
 *
 * <a name="specifyingChosenStyles"></a>
 * <h2>Specifying chosen styles</h2>
 *
 * <p>
 * You can optionally specify chosen styles for a ScalaTest run. ScalaTest supports different styles of
 * testing so that different teams can use the style or styles that best suits their situation and culture. But
 * in any one project, it is recommended you decide on one main style for unit testing, and
 * consistently use only that style for unit testing throughout the project. If you also have integration
 * tests in your project, you may wish to pick a different style for them than you are using for unit testing.
 * You may want to allow certain styles to be used in special testing situations on a project, but in general,
 * it is best to minimize the styles used in any given project to a few, or one.
 * </p>
 *
 * <p>
 * To facilitate the communication and enforcement of a team's style choices for a project, you can
 * specify the chosen styles in your project build. If chosen styles is defined, ScalaTest style traits that are
 * not among the chosen list will abort with a message complaining that the style trait is not one of the
 * chosen styles. The style name for each ScalaTest style trait is its fully qualified name. For example,
 * to specify that <code>org.scalatest.FunSpec</code> as your chosen style you'd pass this to
 * <code>Runner</code>:
 * </p>
 *
 * <pre class="stExamples">-y org.scalatest.FunSpec</pre>
 *
 * <p>
 * If you wanted <code>org.scalatest.FunSpec</code> as your main unit testing style, but also wanted to
 * allow <code>PropSpec</code> for test matrixes and <code>FeatureSpec</code> for
 * integration tests, you would write:
 * </p>
 *
 * <pre class="stExamples">-y org.scalatest.FunSpec -y org.scalatest.PropSpec -y org.scalatest.FeatureSpec</pre>
 *
 * <p>
 * To select <code>org.scalatest.FlatSpec</code> as your main unit testing style, but allow
 * <code>org.scalatest.fixture.FlatSpec</code> for multi-threaded unit tests, you'd write:
 * </p>
 *
 * <pre class="stExamples">-y org.scalatest.FlatSpec -y org.scalatest.fixture.FlatSpec</pre>
 *
 * <p>
 * The style name for a suite is obtained by invoking its <code>styleName</code> method. Custom style
 * traits can override this method so that a custom style can participate in the chosen styles list.
 * </p>
 *
 * <p>
 * Because ScalaTest is so customizable, a determined programmer could circumvent
 * the chosen styles check, but in practice <code>-y</code> should be persuasive enough tool
 * to keep most team members in line.
 * </p>
 *
 * <a name="selectingSuitesAndTests"></a>
 * <h2>Selecting suites and tests</h2>
 *
 * <p>
 * <code>Runner</code> accepts three arguments that facilitate selecting suites and tests: <code>-i</code>, <code>-t</code>, and </code>-z</code>.
 * The <code>-i</code> option enables a suite to be selected by suite ID. This argument is intended to allow tools such as IDEs or build tools to
 * rerun specific tests or suites from information included in the results of a previous run.  A <code>-i</code> must follow a <code>-s</code>
 * that specifies a class with a public, no-arg constructor. The <code>-i</code> parameter can be used, for example, to rerun a nested suite that
 * declares no zero-arg constructor, which was created by containing suite that does declare a no-arg constructor. In this case, <code>-s</code> would be
 * used to specify the class ScalaTest can instantiate directly, the containing suite that has a public, no-arg constructor, and <code>-i</code> would be
 * used to select the desired nested suite. One important use case for <code>-i</code> is to enable such a nested suite that aborted during the previous run
 * to be rerun. <!-- TODO: Need to point them to more info, maybe in SuiteMixin's rerunner method description? -->
 * </p>
 *
 * <p>
 * The <code>-t</code> argument allows a test to be selected by its (complete) test name. Like <code>-i</code>, the <code>-t</code> argument is primarily intented
 * to be used by tools such as IDEs or build tools, to rerun selected tests based on information obtained from the results of a previous run.
 * For example, <code>-t</code> could be used to rerun a test that failed in the previous run.
 * The <code>-t</code> argument can be used directly by users, but because descriptive test names are usually rather long, the <code>-z</code> argument (described next), will
 * usually be a more practical choice for users. If a <code>-t</code> follows either <code>-s</code> or <code>-i</code>, then it only applies to the suite
 * identified.  If it is specified independent of a <code>-s</code> or <code>-i</code>, then discovery is performed to find all Suites containing the test name.
 * </p>
 *
 * <p>
 * The <code>-z</code> option allows tests to be selected by a simplified wildcard: any test whose name includes the substring specified after <code>-z</code>
 * will be selected. For example, <code>-z popped</code> would select tests named <code>"An empty stack should complain when popped"</code> and <code>"A non-empty stack
 * should return the last-pushed value when popped</code>, but not <code>"An empty stack should be empty"</code>. In short, <code>-z popped</code> would select any
 * tests whose name includes the substring <code>"popped"</code>, and not select any tests whose names don't include <code>"popped"</code>. This simplified
 * approach to test name wildcards, which was suggested by Mathias Doenitz, works around the difficulty of finding an actual wildcard character that will work
 * reliably on different operating systems.  Like <code>-t</code>, if <code>-z</code> follows <code>-s</code> or <code>-i</code>, then it only applies to the Suite specified.  Otherwise discovery is performed to find all Suites containing test names that include the substring.
 * </p>
 *
 * <a name="scalingTimeSpans"></a>
 * <h2>Specifying a span scale factor</h2>
 *
 * <p>
 * If you specify a integer or floating point <em>span scale factor</em> with <code>-F</code>, trait <a href="../concurrent/ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>
 * trait will  return the specified value from its implementation of <code>spanScaleFactor</code>. This allows you to tune the "patience" of a run (how long to wait
 * for asynchronous operations) from the command line. For more information, see the documentation for trait <a href="../concurrent/ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>.
 * </p>
 *
 * <a name="specifyingTestNGXML"></a>
 * <h2>Specifying TestNG XML config file paths</h2>
 *
 * <p>
 * If you specify one or more file paths with <code>-b</code> (b for Beust, the last name of TestNG's creator), <code>Runner</code> will create a <code>org.scalatest.testng.TestNGWrapperSuite</code>,
 * passing in a <code>List</code> of the specified paths. When executed, the <code>TestNGWrapperSuite</code> will create one <code>TestNG</code> instance
 * and pass each specified file path to it for running. If you include <code>-b</code> arguments, you must include TestNG's jar file on the class path or runpath.
 * The <code>-b</code> argument will enable you to run existing <code>TestNG</code> tests, including tests written in Java, as part of a ScalaTest run.
 * You need not use <code>-b</code> to run suites written in Scala that extend <code>TestNGSuite</code>. You can simply run such suites with 
 * <code>-s</code>, <code>-m</code>, or </code>-w</code> parameters.
 * </p>
 *
 * <a name="specifyingJUnitTests"></a>
 * <h2>Specifying JUnit tests</h2>
 *
 * <p>
 * JUnit tests, including ones written in Java, may be run by specifying
 * <code>-j classname</code>, where the classname is a valid JUnit class
 * such as a TestCase, TestSuite, or a class implementing a static suite()
 * method returning a TestSuite. </p>
 * <p>
 * To use this option you must include a JUnit jar file on your classpath.
 * </p>
 *
 * <a name="memorizingAndRerunning"> </a>
 * <h2>Memorizing and rerunning failed and canceled tests</h2>
 *
 * <p>
 * You can memorize failed and canceled tests using <code>-M</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * -M failed-canceled.txt
 * </pre>
 *
 * All failed and canceled tests will be memorized in <code>failed-canceled.txt</code>, to rerun them again, you use <code>-A</code>:
 *
 * <pre class="stHighlight">
 * -A failed-canceled.txt
 * </pre>
 *
 * <a name="slowpokeNotifications"> </a>
 * <h2>Slowpoke notifications</h2>
 *
 * <p>
 * You can request to recieve periodic notifications of <em>slowpokes</em>, tests that have been running longer than a given amount of time, specified in
 * seconds by the first integer after <code>-W</code>, the <em>delay</em>.
 * You specify the period between slowpoke notifications in seconds with the second integer after <code>-W</code>, the <em>period</em>. Thus to receive
 * notifications very minute of tests that have been running longer than two minutes, you'd use:
 * </p>
 * 
 * <pre class="stGray">
 * <code>-W 120 60</code>
 * </pre>
 *
 * <p>
 * Slowpoke notifications will be sent via <a href="../events/AlertProvided.html"><code>AlertProvided</code></a> events. The standard out reporter, for example, 
 * will report such notifications like:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stYellow">*** Test still running after 2 minutes, 13 seconds: suite name: ExampleSpec, test name: An egg timer should take 10 minutes.</span>
 * </pre>
 *
 * @author Bill Venners
 * @author George Berger
 * @author Josh Cough
 * @author Chee Seng
 */
object Runner {

  private val RUNNER_JFRAME_START_X: Int = 150
  private val RUNNER_JFRAME_START_Y: Int = 100
  
  @volatile private[scalatest] var spanScaleFactor: Double = 1.0

  private final val DefaultNumFilesToArchive = 2
  
  //                     TO
  // We always include a PassFailReporter on runs in order to determine
  // whether or not all tests passed.
  //
  // The thread that calls Runner.run() will either start a GUI, if a graphic
  // reporter was requested, or just run the tests itself. If a GUI is started,
  // an event handler thread will get going, and it will start a RunnerThread,
  // which will actually do the running. The GUI can repeatedly start RunnerThreads
  // and RerunnerThreads, until the GUI is closed. If -P is specified, that means
  // the tests should be run concurrently, which in turn means a Distributor will
  // be passed to the execute method of the Suites, which will in turn populate
  // it with its nested suites instead of executing them directly in the same
  // thread. The Distributor works in conjunction with a pool of threads that
  // will take suites out of the distributor queue and execute them. The DispatchReporter
  // will serialize all reports via an actor, which because that actor uses receive
  // not react, will have its own thread. So the DispatchReporter actor's thread will
  // be the one that actually invokes TestFailed, RunAborted, etc., on this PassFailReporter.
  // The thread that invoked Runner.run(), will be the one that calls allTestsPassed.
  //
  // The thread that invoked Runner.run() will be the one to instantiate the PassFailReporter
  // and in its primary constructor acquire the single semaphore permit. This permit will
  // only be released by the DispatchReporter's actor thread when a runAborted, runStopped,
  // or runCompleted is invoked. allTestsPassed will block until it can reacquire the lone
  // semaphore permit. Thus, a PassFailReporter can just be used for one run, then it is
  // spent. A new PassFailReporter is therefore created each time the Runner.run() method is invoked.
  //
  private class PassFailReporter extends Reporter {

    @volatile private var failedAbortedOrStopped = false
    private val runDoneSemaphore = new Semaphore(1)
    runDoneSemaphore.acquire()

    override def apply(event: Event) {
      event match {
        case _: TestFailed =>
          failedAbortedOrStopped = true

        case _: RunAborted =>
          failedAbortedOrStopped = true
          runDoneSemaphore.release()

        case _: SuiteAborted =>
          failedAbortedOrStopped = true

        case _: RunStopped =>
          failedAbortedOrStopped = true
          runDoneSemaphore.release() 

        case _: RunCompleted =>
          runDoneSemaphore.release()

        case _ =>
      }
    }

    def allTestsPassed = {
      runDoneSemaphore.acquire()
      !failedAbortedOrStopped
    }
  }

  // TODO: I don't think I'm enforcing that properties can't start with "org.scalatest"
  // TODO: I don't think I'm handling rejecting multiple -f/-C with the same arg. -f fred.txt -f fred.txt should
  // fail, as should -C MyReporter -C MyReporter. I'm failing on -o -o, -g -g, and -e -e, but the error messages
  // could indeed be nicer.
  /**
   * Runs a suite of tests, with optional GUI. See the main documentation for this singleton object for the details.
   */
  def main(args: Array[String]) {
    // println("FOR DEBUGGING, THESE ARE THE ARGS PASSED TO main(): " + args.mkString(" "))
    Thread.currentThread.setName("ScalaTest-main")
    val result = 
      if (args.contains("-v") || args.contains("--version")) {
        val version = org.scalatest.ScalaTestVersions.ScalaTestVersion
        val scalaVersion = org.scalatest.ScalaTestVersions.BuiltForScalaVersion
        println("ScalaTest " + version + " (Built for Scala " + scalaVersion + ")")
        runOptionallyWithPassFailReporter(args.filter(arg => arg != "-v" && arg != "--version"), true)
      }
      else
        runOptionallyWithPassFailReporter(args, true)

    if (result)
      System.exit(0)
    else
      System.exit(1)
  }

  /**
   * Runs a suite of tests, with optional GUI. See the main documentation for this singleton object for the details.
   * The difference between this method and <code>main</code> is simply that this method will block until the run
   * has completed, aborted, or been stopped, and return <code>true</code> if all tests executed and passed. In other
   * words, if any test fails, or if any suite aborts, or if the run aborts or is stopped, this method will
   * return <code>false</code>. This value is used, for example, by the ScalaTest ant task to determine whether
   * to continue the build if <code>haltOnFailure</code> is set to <code>true</code>.
   *
   * @return true if all tests were executed and passed.
   */
  def run(args: Array[String]): Boolean = {
    // println("FOR DEBUGGING, THESE ARE THE ARGS PASSED TO run(): " + args.mkString(" "))
    val originalThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName("ScalaTest-run")
      runOptionallyWithPassFailReporter(args, true)
    }
    finally Thread.currentThread.setName(originalThreadName)
  }

  private def runOptionallyWithPassFailReporter(args: Array[String], runWithPassFailReporter: Boolean): Boolean = {

    checkArgsForValidity(args) match {
      case Some(s) => {
        println(s)
        System.exit(1) // TODO: Shouldn't this be returning false?
      }
      case None =>
    }

    val ParsedArgs(
      runpathArgs,
      reporterArgs,
      suiteArgs,
      againArgs,
      junitArgs,
      propertiesArgs,
      tagsToIncludeArgs,
      tagsToExcludeArgs,
      concurrentArgs,
      membersOnlyArgs,
      wildcardArgs,
      testNGArgs,
      suffixes, 
      chosenStyles, 
      spanScaleFactors, 
      testSortingReporterTimeouts,
      slowpokeArgs
    ) = parseArgs(args)

    val fullReporterConfigurations: ReporterConfigurations =
      if (reporterArgs.isEmpty)
        // If no reporters specified, just give them a graphic reporter
        new ReporterConfigurations(Some(GraphicReporterConfiguration(Set())), Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)
      else
        parseReporterArgsIntoConfigurations(reporterArgs)

    val (suitesList: List[SuiteParam], testSpecs: List[TestSpec]) =
      parseSuiteArgs(suiteArgs)
    val agains: List[String] = parseAgainArgs(againArgs)
    val junitsList: List[String] = parseSuiteArgsIntoNameStrings(junitArgs, "-j")
    val runpathList: List[String] = parseRunpathArgIntoList(runpathArgs)
    val propertiesMap: ConfigMap = parsePropertiesArgsIntoMap(propertiesArgs)
    val tagsToInclude: Set[String] = parseCompoundArgIntoSet(tagsToIncludeArgs, "-n")
    val tagsToExclude: Set[String] = parseCompoundArgIntoSet(tagsToExcludeArgs, "-l")
    val concurrent: Boolean = !concurrentArgs.isEmpty
    val concurrentConfig: ConcurrentConfig = parseConcurrentConfig(concurrentArgs)
    val membersOnlyList: List[String] = parseSuiteArgsIntoNameStrings(membersOnlyArgs, "-m")
    val wildcardList: List[String] = parseSuiteArgsIntoNameStrings(wildcardArgs, "-w")
    val testNGList: List[String] = parseSuiteArgsIntoNameStrings(testNGArgs, "-b")
    val chosenStyleSet: Set[String] = parseChosenStylesIntoChosenStyleSet(chosenStyles, "-y")
    val slowpokeConfig: Option[SlowpokeConfig] = parseSlowpokeConfig(slowpokeArgs)
    spanScaleFactor = parseDoubleArgument(spanScaleFactors, "-F", 1.0)
    testSortingReporterTimeout = Span(parseDoubleArgument(testSortingReporterTimeouts, "-T", 2.0), Seconds)

    // If there's a graphic reporter, we need to leave it out of
    // reporterSpecs, because we want to pass all reporterSpecs except
    // the graphic reporter's to the RunnerJFrame (because RunnerJFrame *is*
    // the graphic reporter).
    val reporterConfigs: ReporterConfigurations =
      fullReporterConfigurations.graphicReporterConfiguration match {
        case None => fullReporterConfigurations
        case Some(grs) => {
          new ReporterConfigurations(
            None,
            fullReporterConfigurations.fileReporterConfigurationList,
            fullReporterConfigurations.memoryReporterConfigurationList,
            fullReporterConfigurations.junitXmlReporterConfigurationList,
            //fullReporterConfigurations.dashboardReporterConfigurationList,
            //fullReporterConfigurations.xmlReporterConfigurationList,
            fullReporterConfigurations.standardOutReporterConfiguration,
            fullReporterConfigurations.standardErrReporterConfiguration,
            fullReporterConfigurations.htmlReporterConfigurationList,
            fullReporterConfigurations.customReporterConfigurationList, 
            fullReporterConfigurations.xmlSocketReporterConfigurationList, 
            fullReporterConfigurations.socketReporterConfigurationList
          )
        }
      }

    val passFailReporter = if (runWithPassFailReporter) Some(new PassFailReporter) else None

    if (propertiesMap.isDefinedAt(CHOSEN_STYLES))
      throw new IllegalArgumentException("Property name '" + CHOSEN_STYLES + "' is used by ScalaTest, please choose other property name.")
    val configMap: ConfigMap = 
      if (chosenStyleSet.isEmpty)
        propertiesMap
      else
        propertiesMap + (CHOSEN_STYLES -> chosenStyleSet)

    val (detectSlowpokes: Boolean, slowpokeDetectionDelay: Long, slowpokeDetectionPeriod: Long) =
      slowpokeConfig match {
        case Some(SlowpokeConfig(delayInMillis, periodInMillis)) => (true, delayInMillis, periodInMillis)
        case _ => (false, 60000L, 60000L)
      }
    fullReporterConfigurations.graphicReporterConfiguration match {
      case Some(GraphicReporterConfiguration(configSet)) => {
        val graphicEventsToPresent: Set[EventToPresent] = EventToPresent.allEventsToPresent filter
          (if (configSet.contains(FilterTestStarting)) {_ != PresentTestStarting} else etp => true) filter
          (if (configSet.contains(FilterTestSucceeded)) {_ != PresentTestSucceeded} else etp => true) filter
          (if (configSet.contains(FilterTestIgnored)) {_ != PresentTestIgnored} else etp => true) filter
          (if (configSet.contains(FilterTestPending)) {_ != PresentTestPending} else etp => true) filter
          (if (configSet.contains(FilterScopeOpened)) {_ != PresentScopeOpened} else etp => true) filter
          (if (configSet.contains(FilterScopeClosed)) {_ != PresentScopeClosed} else etp => true) filter
          (if (configSet.contains(FilterScopePending)) {_ != PresentScopePending} else etp => true) filter
          (if (configSet.contains(FilterSuiteStarting)) {_ != PresentSuiteStarting} else etp => true) filter
          (if (configSet.contains(FilterSuiteCompleted)) {_ != PresentSuiteCompleted} else etp => true) filter
          (if (configSet.contains(FilterInfoProvided)) {_ != PresentInfoProvided} else etp => true) filter 
          (if (configSet.contains(FilterMarkupProvided)) {_ != PresentMarkupProvided} else etp => true)

        val abq = new ArrayBlockingQueue[RunnerJFrame](1)
        usingEventDispatchThread {
          val rjf = new RunnerJFrame(
            graphicEventsToPresent,
            reporterConfigs,
            suitesList,
            agains,
            testSpecs,
            junitsList,
            runpathList,
            tagsToInclude,
            tagsToExclude,
            configMap,
            concurrent,
            membersOnlyList,
            wildcardList,
            testNGList,
            passFailReporter,
            concurrentConfig,
            suffixes,
            chosenStyleSet,
            detectSlowpokes,
            slowpokeDetectionDelay,
            slowpokeDetectionPeriod
          )
          rjf.setLocation(RUNNER_JFRAME_START_X, RUNNER_JFRAME_START_Y)
          rjf.setVisible(true)
          rjf.prepUIForRunning()
          rjf.runFromGUI()
          abq.put(rjf)
        }
        // To get the Ant task to work, the main thread needs to block until
        // The GUI window exits.
        val rjf = abq.take()
        rjf.blockUntilWindowClosed()
      }
      case None => { // Run the test without a GUI
        withClassLoaderAndDispatchReporter(
          runpathList,
          reporterConfigs,
          None,
          passFailReporter,
          detectSlowpokes,
          slowpokeDetectionDelay,
          slowpokeDetectionPeriod
       ) { (loader, dispatchReporter) =>
          doRunRunRunDaDoRunRun(
            dispatchReporter,
            suitesList,
            agains,
            testSpecs,
            junitsList,
            Stopper.default,
            tagsToInclude,
            tagsToExclude,
            configMap,
            concurrent,
            membersOnlyList,
            wildcardList,
            testNGList,
            runpathList,
            loader,
            new RunDoneListener {},
            1,
            concurrentConfig,
            suffixes,
            chosenStyleSet
          )
        }
      }
    }
    
    passFailReporter match {
      case Some(pfr) => pfr.allTestsPassed
      case None => false
    }
  }

  // For debugging.
/*
  private[scalatest] def printOpts(opt: EventToPresent.Set32) {
    if (opt.contains(EventToPresent.PresentRunStarting))
      println("PresentRunStarting")
    if (opt.contains(EventToPresent.PresentTestStarting))
      println("PresentTestStarting")
    if (opt.contains(EventToPresent.PresentTestSucceeded))
      println("PresentTestSucceeded")
    if (opt.contains(EventToPresent.PresentTestFailed))
      println("PresentTestFailed")
    if (opt.contains(EventToPresent.PresentTestIgnored))
      println("PresentTestIgnored")
    if (opt.contains(EventToPresent.PresentSuiteStarting))
      println("PresentSuiteStarting")
    if (opt.contains(EventToPresent.PresentSuiteCompleted))
      println("PresentSuiteCompleted")
    if (opt.contains(EventToPresent.PresentSuiteAborted))
      println("PresentSuiteAborted")
    if (opt.contains(EventToPresent.PresentInfoProvided))
      println("PresentInfoProvided")
    if (opt.contains(EventToPresent.PresentRunStopped))
      println("PresentRunStopped")
    if (opt.contains(EventToPresent.PresentRunCompleted))
      println("PresentRunCompleted")
    if (opt.contains(EventToPresent.PresentRunAborted))
      println("PresentRunAborted")
  }
*/
  
  // We number our named threads so that people can keep track
  // of it as it goes through different suites. But in case the
  // multiple doRunRunRunDaDoRunRun's get called, we want to
  // use different numbers. So this is a "global" count in Runner.
  private val atomicThreadCounter = new AtomicInteger

  private[scalatest] def doRunRunRunDaDoRunRun(
    dispatch: DispatchReporter,
    suitesList: List[SuiteParam],
    agains: List[String],
    testSpecs: List[TestSpec],
    junitsList: List[String],
    stopper: Stopper,
    tagsToIncludeSet: Set[String], 
    tagsToExcludeSet: Set[String], 
    configMap: ConfigMap,
    concurrent: Boolean,
    membersOnlyList: List[String],
    wildcardList: List[String],
    testNGList: List[String],
    runpath: List[String],
    loader: ClassLoader,
    doneListener: RunDoneListener,
    runStamp: Int,
    concurrentConfig: ConcurrentConfig,
    suffixes: Option[Pattern],
    chosenStyleSet: Set[String]
  ): Unit = {

    // TODO: add more, and to RunnerThread too
    requireNonNull(dispatch,
                   suitesList,
                   agains,
                   testSpecs,
                   junitsList,
                   stopper,
                   tagsToIncludeSet,
                   tagsToExcludeSet,
                   configMap,
                   membersOnlyList,
                   wildcardList,
                   testNGList,
                   runpath,
                   loader,
                   doneListener,
                   chosenStyleSet)

    val (globSuites, nonGlobSuites) = suitesList.partition(_.isGlob)

    var tracker = new Tracker(new Ordinal(runStamp))

    //
    // Generates SuiteConfigs for Suites found via discovery.
    //
    def genDiscoSuites: List[SuiteConfig] = {
      val emptyDynaTags = DynaTags(Map.empty[String, Set[String]], Map.empty[String, Map[String, Set[String]]])

      //
      // If user specified any -t or -z arguments independent of
      // a Suite name, or any members-only or wildcard args, or
      // suite args with glob characters, then we need to do
      // discovery to find the Suites associated with those
      // args.
      //
      val discoArgsArePresent =
        !membersOnlyList.isEmpty || !wildcardList.isEmpty ||
        !testSpecs.isEmpty || !globSuites.isEmpty

      //
      // If user specified any specific Suites to run, then we don't
      // have to do any discovery unless discoArgsArePresent.
      //
      val suiteArgsArePresent =
        !nonGlobSuites.isEmpty || !junitsList.isEmpty || !testNGList.isEmpty ||
        !agains.isEmpty

      if (suiteArgsArePresent && !discoArgsArePresent) {
        Nil // No DiscoverySuites in this case. Just run Suites
            // named with -s or -j or -b
      }
      else {
        val discoveryStartTime = System.currentTimeMillis
        dispatch(DiscoveryStarting(tracker.nextOrdinal(), configMap))

        val accessibleSuites: Set[String] =
          discoverSuiteNames(runpath, loader, suffixes)

        val discoSuites =
          if (!discoArgsArePresent && !suiteArgsArePresent) {
            // In this case, they didn't specify any -w, -m, -s,
            // -j or -b on the command line, so the default is to
            // run any accessible Suites discovered on the runpath
            List(SuiteConfig(new DiscoverySuite("", accessibleSuites, true, loader), emptyDynaTags, false, false))
          }
          else {
            val membersOnlyInstances =
              for (membersOnlyName <- membersOnlyList)
                yield SuiteConfig(new DiscoverySuite(membersOnlyName, accessibleSuites, false, loader), emptyDynaTags, false, false)
  
            val wildcardInstances =
              for (wildcardName <- wildcardList)
                yield SuiteConfig(new DiscoverySuite(wildcardName, accessibleSuites, true, loader), emptyDynaTags, false, false)
  
            val testSpecSuiteParams =
              SuiteDiscoveryHelper.discoverTests(
                testSpecs, accessibleSuites, loader)
  
            val testSpecInstances = 
              for (suiteParam <- testSpecSuiteParams)
                yield genSuiteConfig(suiteParam, loader)
  
            val deglobbedSuiteParams: List[SuiteParam] =
              deglobSuiteParams(globSuites, accessibleSuites)
  
            val globInstances = 
              for (suiteParam <- deglobbedSuiteParams)
                yield genSuiteConfig(suiteParam, loader)
  
            membersOnlyInstances ::: wildcardInstances ::: testSpecInstances ::: globInstances
          }

        val discoveryDuration = System.currentTimeMillis - discoveryStartTime
        dispatch(
          DiscoveryCompleted(tracker.nextOrdinal(), Some(discoveryDuration)))

        discoSuites
      }
    }

    // suites specified by name, either directly via -s or in a file via -A
    val againSuites = readMemoryFiles(agains, dispatch, tracker)
    val specificSuites = nonGlobSuites ::: againSuites

    val runStartTime = System.currentTimeMillis
    
    try {
      val loadProblemsExist =
        try {
          val unrunnableList = specificSuites.filter{ suiteParam => 
            val className = suiteParam.className
            loader.loadClass(className) // Check if the class exist, so if not we get the nice cannot load suite error message.
            !isAccessibleSuite(className, loader) && !isRunnable(className, loader)
          }
          if (!unrunnableList.isEmpty) {
            val names = for (suiteParam <- unrunnableList) yield " " + suiteParam.className
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.nonSuite + names.mkString(", "), None))
            true
          }
          else {
            false
          }
        }
        catch {
          case e: ClassNotFoundException => {
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotLoadSuite(e.getMessage), Some(e)))
            true
          }
        }

      if (!loadProblemsExist) {
        try {
          val namedSuiteInstances: List[SuiteConfig] =
            for (suiteParam <- specificSuites)
              yield genSuiteConfig(suiteParam, loader)
          
          val emptyDynaTags = DynaTags(Map.empty[String, Set[String]], Map.empty[String, Map[String, Set[String]]])

          val junitSuiteInstances: List[SuiteConfig] =
            for (junitClassName <- junitsList)
              yield SuiteConfig(new JUnitWrapperSuite(junitClassName, loader), emptyDynaTags, false, true) // JUnit suite should exclude nested suites

          val testNGWrapperSuiteList: List[SuiteConfig] =
            if (!testNGList.isEmpty)
              List(SuiteConfig(new TestNGWrapperSuite(testNGList), emptyDynaTags, false, true)) // TestNG suite should exclude nested suites
            else
              Nil

          val discoSuiteInstances = genDiscoSuites

          val suiteInstances: List[SuiteConfig] = namedSuiteInstances ::: junitSuiteInstances ::: discoSuiteInstances ::: testNGWrapperSuiteList

          val testCountList =
            for (suiteConfig <- suiteInstances)
              yield { 
              val tagsToInclude = if (suiteConfig.requireSelectedTag) tagsToIncludeSet ++ Set(SELECTED_TAG) else tagsToIncludeSet
              val filter = Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExcludeSet, suiteConfig.excludeNestedSuites, suiteConfig.dynaTags)
              suiteConfig.suite.expectedTestCount(filter)
            }
  
          def sumInts(list: List[Int]): Int =
            list match {
              case Nil => 0
              case x :: xs => x + sumInts(xs)
            }

          val expectedTestCount = sumInts(testCountList)

          dispatch(RunStarting(tracker.nextOrdinal(), expectedTestCount, configMap))
          
          if (concurrent) {

            // Because some tests may do IO, will create a pool of 2 times the number of processors reported
            // by the Runtime's availableProcessors method.
            val poolSize =
              if (concurrentConfig.numThreads > 0) concurrentConfig.numThreads
              else Runtime.getRuntime.availableProcessors * 2

            val distributedSuiteSorter = 
              if (concurrentConfig.enableSuiteSortingReporter)
                Some(new SuiteSortingReporter(dispatch, Span(testSortingReporterTimeout.millisPart + 1000, Millis), System.err))
              else
                None
              
            val concurrentDispatch = 
              distributedSuiteSorter match {
                case Some(dss) => dss
                case None => dispatch
              }
                
            val threadFactory =
              new ThreadFactory {
                val defaultThreadFactory = Executors.defaultThreadFactory
                def newThread(runnable: Runnable): Thread = {
                  val thread = defaultThreadFactory.newThread(runnable)
                  thread.setName("ScalaTest-" + atomicThreadCounter.incrementAndGet())
                  thread
                }
              }
            val execSvc: ExecutorService = Executors.newFixedThreadPool(poolSize, threadFactory)
            try {

              val distributor = new ConcurrentDistributor(Args(dispatch, stopper, Filter(if (tagsToIncludeSet.isEmpty) None else Some(tagsToIncludeSet), tagsToExcludeSet), configMap, None, tracker, chosenStyleSet), execSvc)
              if (System.getProperty("org.scalatest.tools.Runner.forever", "false") == "true") {

                while (true) {
                  for (suiteConfig <- suiteInstances) {
                    val tagsToInclude = if (suiteConfig.requireSelectedTag) tagsToIncludeSet ++ Set(SELECTED_TAG) else tagsToIncludeSet
                    val filter = Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExcludeSet, suiteConfig.excludeNestedSuites, suiteConfig.dynaTags)
                    val runArgs = Args(concurrentDispatch, stopper, filter, configMap, Some(distributor), tracker.nextTracker, chosenStyleSet, false, None, distributedSuiteSorter)
                    distributor.apply(suiteConfig.suite, runArgs)
                  }
                  distributor.waitUntilDone()
                }
              }
              else {
                for (suiteConfig <- suiteInstances) {
                  val tagsToInclude = if (suiteConfig.requireSelectedTag) tagsToIncludeSet ++ Set(SELECTED_TAG) else tagsToIncludeSet
                  val filter = Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExcludeSet, suiteConfig.excludeNestedSuites, suiteConfig.dynaTags)
                  val runArgs = Args(concurrentDispatch, stopper, filter, configMap, Some(distributor), tracker.nextTracker, chosenStyleSet, false, None, distributedSuiteSorter)
                  distributor.apply(suiteConfig.suite, runArgs)
                }
                distributor.waitUntilDone()
              }
            }
            finally {
              execSvc.shutdown()
            }
          }
          else {
            for (suiteConfig <- suiteInstances) {
              val tagsToInclude = if (suiteConfig.requireSelectedTag) tagsToIncludeSet ++ Set(SELECTED_TAG) else tagsToIncludeSet
              val filter = Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExcludeSet, suiteConfig.excludeNestedSuites, suiteConfig.dynaTags)
              val runArgs = Args(dispatch, stopper, filter, configMap, None, tracker, chosenStyleSet)
              val status = new ScalaTestStatefulStatus()
              val suiteRunner = new SuiteRunner(suiteConfig.suite, runArgs, status)
              suiteRunner.run()
            }
          }

          val duration = System.currentTimeMillis - runStartTime
          if (stopper.stopRequested) {
            dispatch(RunStopped(tracker.nextOrdinal(), Some(duration)))
          }
          else {
            dispatch(RunCompleted(tracker.nextOrdinal(), Some(duration)))
          }
        }
        catch {
          case e: InstantiationException =>
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotInstantiateSuite(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
          case e: IllegalAccessException =>
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotInstantiateSuite(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
          case e: NoClassDefFoundError =>
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotLoadClass(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
          case e: Throwable =>
            dispatch(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e), Some(System.currentTimeMillis - runStartTime)))
        }
      }
    }
    finally {
      dispatch.dispatchDisposeAndWaitUntilDone()
      doneListener.done()
    }
  }

  //
  // Creates a SuiteParam for each Suite found that matches one
  // of the patterns in a list of SuiteParams containing globs.
  //
  private[tools] def deglobSuiteParams(globsList: List[SuiteParam],
                                       accessibleSuites: Set[String]):
  List[SuiteParam] =
    for {
      suiteParam <- globsList
      name <- accessibleSuites
      if suiteParam.matches(name)
    }
    yield suiteParam.copy(className = name)

  //
  // Reads each specified file and generates a SuiteParam object
  // for each entry in file.  Files are of the format created when
  // -M option is specified to record failed/canceled/aborted
  // tests so they can be run again later.
  //
  private[tools] def readMemoryFiles(fileNames: List[String],
                                     reporter: Reporter,
                                     tracker: Tracker):
  List[SuiteParam] =
  {
    val mementos =
      for {
        fileName <- fileNames
        memento <- Memento.readFromFile(fileName)
      }
      yield memento

    val (unrerunnables, rerunnables) = mementos.partition(_.className == None)

    for (memento <- unrerunnables)
      reporter.apply(
        AlertProvided(
          tracker.nextOrdinal,
          Resources.cannotRerun(memento.eventName, memento.suiteId,
                    memento.testName),
          None))

    rerunnables.map(_.toSuiteParam)
  }

  private[tools] def genSuiteConfig(suiteParam: SuiteParam, loader: ClassLoader): SuiteConfig = {
    val suiteClassName = suiteParam.className
    val clazz = loader.loadClass(suiteClassName)
    val wrapWithAnnotation = clazz.getAnnotation(classOf[WrapWith])
    val suiteInstance = 
      if (wrapWithAnnotation == null) 
        clazz.newInstance.asInstanceOf[Suite]
      else {
        val suiteClazz = wrapWithAnnotation.value
        val constructorList = suiteClazz.getDeclaredConstructors()
        val constructor = constructorList.find { c => 
          val types = c.getParameterTypes
          types.length == 1 && types(0) == classOf[java.lang.Class[_]]
        }
        constructor.get.newInstance(clazz).asInstanceOf[Suite]
      }
    
    if (suiteParam.testNames.length == 0 && suiteParam.wildcardTestNames.length == 0 && suiteParam.nestedSuites.length == 0)
      SuiteConfig(suiteInstance, new DynaTags(Map.empty, Map.empty), false, false) // -s suiteClass, no dynamic tagging required.
    else {
      val nestedSuites = suiteParam.nestedSuites
      
      val (selectSuiteList, selectTestList) = nestedSuites.partition(ns => ns.testNames.length == 0 || ns.wildcardTestNames.length == 0)
      val suiteDynaTags: Map[String, Set[String]] = Map() ++ selectSuiteList.map(ns => (ns.suiteId -> Set(SELECTED_TAG)))
      
      val suiteExactTestDynaTags: Map[String, Map[String, Set[String]]] = 
        if (suiteParam.testNames.length > 0) 
          Map(suiteInstance.suiteId -> (Map() ++ suiteParam.testNames.map(tn => (tn -> Set(SELECTED_TAG)))))
        else 
          Map.empty
      
      val suiteWildcardTestDynaTags: Map[String, Map[String, Set[String]]] = 
        if (suiteParam.wildcardTestNames.length > 0) {
          val wildcardTestNames = suiteParam.wildcardTestNames
          val allTestNames = suiteInstance.testNames
          val wildcardTestTags = Map() ++ allTestNames.filter(tn => wildcardTestNames.find(wc => tn.contains(wc)).isDefined)
                                              .map(tn => (tn -> Set(SELECTED_TAG)))
          Map(suiteInstance.suiteId -> wildcardTestTags)
        }
        else
          Map.empty
          
      def getNestedSuiteSelectedTestNames(nestedSuite: NestedSuiteParam): Array[String] = {
        if (nestedSuite.wildcardTestNames.length == 0)
          nestedSuite.testNames
        else {
          val wildcardTestNames = nestedSuite.wildcardTestNames
          val allTestNames = suiteInstance.testNames
          nestedSuite.testNames ++ allTestNames.filter(tn => wildcardTestNames.find(wc => tn.contains(wc)).isDefined)
        }
      }
      
      val nestedSuitesTestDynaTags: Map[String, Map[String, Set[String]]] 
        = Map() ++ selectTestList.map(ns => (ns.suiteId -> (Map() ++ getNestedSuiteSelectedTestNames(ns).map(tn => (tn, Set(SELECTED_TAG))))))
        
      val testDynaTags = mergeMap[String, Map[String, Set[String]]](List(suiteExactTestDynaTags, suiteWildcardTestDynaTags, nestedSuitesTestDynaTags)) { (suiteTestMap1, suiteTestMap2) => 
                           mergeMap[String, Set[String]](List(suiteTestMap1, suiteTestMap2)) { (tagSet1, tagSet2) =>
                             tagSet1 ++ tagSet2
                           }
                         }
      // Only exclude nested suites when using -s XXX -t XXXX, or -s XXX -z XXX
      val excludeNestedSuites = suiteParam.testNames.length > 0 && nestedSuites.length == 0
      SuiteConfig(suiteInstance, new DynaTags(suiteDynaTags, testDynaTags), true, excludeNestedSuites)
    }
  }

  private[scalatest] def excludesWithIgnore(excludes: Set[String]) = excludes + "org.scalatest.Ignore"

  private[scalatest] def withClassLoaderAndDispatchReporter(
    runpathList: List[String],
    reporterSpecs: ReporterConfigurations,
    graphicReporter: Option[Reporter],
    passFailReporter: Option[Reporter],
    detectSlowpokes: Boolean,
    slowpokeDetectionDelay: Long,
    slowpokeDetectionPeriod: Long
  )(f: (ClassLoader, DispatchReporter) => Unit): Unit = {

    val loader: ClassLoader = getRunpathClassLoader(runpathList)
    try {
      Thread.currentThread.setContextClassLoader(loader)
      try {
        val dispatchReporter = ReporterFactory.getDispatchReporter(reporterSpecs, graphicReporter, passFailReporter, loader, None, detectSlowpokes, slowpokeDetectionDelay, slowpokeDetectionPeriod)
        try {
          f(loader, dispatchReporter)
        }
        finally {
          dispatchReporter.dispatchDisposeAndWaitUntilDone()
        }
      }
      catch {
        // getDispatchReporter may complete abruptly with an exception, if there is an problem trying to load
        // or instantiate a custom reporter class.
        case ex: Throwable => {
          System.err.println(Resources.bigProblemsMaybeCustomReporter)
          ex.printStackTrace(System.err)
        }
      }
    }
    finally {
      // eventually call close on the RunpathClassLoader
    }
  }

  private[scalatest] def getRunpathClassLoader(runpathList: List[String]): ClassLoader = {

    requireNonNull(runpathList)
    if (runpathList.isEmpty) {
      classOf[Suite].getClassLoader // Could this be null technically?
    }
    else {
      val urlsList: List[URL] =
        for (raw <- runpathList) yield {
          try {
            new URL(raw)
          }
          catch {
            case murle: MalformedURLException => {
  
              // Assume they tried to just pass in a file name
              val file: File = new File(raw)
  
              // file.toURL may throw MalformedURLException too, but for now
              // just let that propagate up.
              file.toURI.toURL // If a dir, comes back terminated by a slash
            }
          }
        }
  
      // Here is where the Jini preferred class loader stuff went.

      // Tell the URLConnections to not use caching, so that repeated runs and reruns actually work
      // on the latest binaries.
      for (url <- urlsList) {
        try {
          url.openConnection.setDefaultUseCaches(false)
        }
        catch {
          case e: IOException => // just ignore these
        }
      }

      new URLClassLoader(urlsList.toArray, classOf[Suite].getClassLoader)
    }
  }

  private[scalatest] def usingEventDispatchThread(f: => Unit): Unit = {
    SwingUtilities.invokeLater(
      new Runnable() {
        def run() {
          f
        }
      }
    )
  }
}

