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
package org.scalatest.osgi

import javax.inject.Inject
import scala.collection.JavaConverters._
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam.CoreOptions._
import org.ops4j.pax.exam.junit.{JUnit4TestRunner, Configuration}
import org.osgi.framework.{Bundle, BundleContext}
import org.osgi.framework.wiring._
import org.scalatest.junit._


@RunWith(classOf[JUnit4TestRunner])
class OsgiSuite extends JUnitSuite with ShouldMatchersForJUnit {

  @Inject var context: BundleContext = _

  @Configuration def config = options(
    junitBundles,
    bundle("file:target/dist/lib/scalatest.jar"),
    bundle("file:target/dist/lib/scalactic.jar"),
    scalaBundles
  )

  private def scalaBundles = composite(
    mavenBundle.groupId("org.scala-lang").artifactId("scala-library").version(compiledAgainstScalaVersionString),
    mavenBundle.groupId("org.scala-lang").artifactId("scala-reflect").version(compiledAgainstScalaVersionString),
    mavenBundle.groupId("org.scala-lang").artifactId("scala-compiler").version(compiledAgainstScalaVersionString)
  )

  @Test def verifyScalaTestBundlesResolve {
    bundleNamed("org.scalatest") should be ('defined)
    bundleNamed("org.scalactic") should be ('defined)
  }

  @Test def scalaPackageImportsUseVersionRangeForCurrentMinorUpToNextMinor {
    checkScalaPackage(bundleNamed("org.scalatest").get)
    checkScalaPackage(bundleNamed("org.scalactic").get)
  }

  private def bundleNamed(symbolicName: String): Option[Bundle] =
    context.getBundles.find { _.getSymbolicName == symbolicName }

  private def checkScalaPackage(bundle: Bundle) = {
    def packageName(wire: BundleWire) = wire.getCapability.getAttributes.get(BundleRevision.PACKAGE_NAMESPACE)
    def scalaWire(wire: BundleWire) = packageName(wire) == "scala"
    val scalaPackageImportFilter = bundle.adapt(classOf[BundleWiring]).
      getRequiredWires(BundleRevision.PACKAGE_NAMESPACE).asScala.
      find(scalaWire).
      getOrElse(throw new IllegalStateException("Bundle %s does not import scala package".format(bundle.getSymbolicName))).
      getRequirement.
      getDirectives.asScala.
      get("filter").
      getOrElse(throw new IllegalStateException("Bundle %s imports scala package but does not specify a version range".format(bundle.getSymbolicName)))

    val version = compiledAgainstScalaVersion
    val lowerBound = "%s.%s.0".format(version.major, version.minor)
    val upperBound = "%s.%s.0".format(version.major, version.minor + 1)
    val expectedImportFilter = "(&(osgi.wiring.package=scala)(version>=%s)(!(version>=%s)))".format(lowerBound, upperBound)

    scalaPackageImportFilter should be (expectedImportFilter)
  }

  private def compiledAgainstScalaVersionString: String = {
    scala.util.Properties.
      propOrNone("scala.version").
      getOrElse(throw new IllegalStateException("scala.version system property is not set"))
  }

  private def compiledAgainstScalaVersion: Version = {
    val ScalaVersion = """(\d+)\.(\d+)\.(\d+).*""".r
    val ScalaVersion(major, minor, micro) = compiledAgainstScalaVersionString
    Version(major.toInt, minor.toInt, micro.toInt)
  }

  case class Version(major: Int, minor: Int, micro: Int)
}
