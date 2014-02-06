/*
 * This object converts java source files that define tag classes into
 * scala files that can be used to generate scaladocs.
 *
 * The resulting scala files won't work for actually running code, but
 * they're close enough to get into the scaladocs.
 *
 * The script rewrites java files found under src/main/java.  It looks
 * for files that contain the comment:
 *
 *    "Note: This is actually an annotation defined in Java"
 *
 * It copies them into their relative directories under target/docsrc,
 * removing java annotations and converting them into similar scala code,
 * preserving their header comments so those make it into the scaladocs.
 *
 * E.g. this java code:
 *
 *   @TagAnnotation
 *   @Retention(RetentionPolicy.RUNTIME)
 *   @Target({ElementType.METHOD, ElementType.TYPE})
 *   public @interface Ignore {
 *   }
 *
 * gets transformed to this scala:
 *
 *   trait Ignore extends java.lang.annotation.Annotation 
 */

import java.io.File
import java.io.PrintWriter
import java.util.regex.Pattern
import scala.io.Source
import sbt.file
import sbt.IO.createDirectory

object JavaTagDocumenter {
  val docsrcDir = "target/docsrc"

  //
  // Splits java file's contents into two pieces: a top and body.
  // The top contains everything up through the declared class's name, and
  // the body contains the following curly braces and their contents.
  //
  private def parseContents(className: String, text: String): (String, String) =
  {
    val pat = Pattern.compile("""(?sm)(.*? @interface """ + className +
                              """) *(\{.*\})""")
    val matcher = pat.matcher(text)

    matcher.find()
    (matcher.group(1), matcher.group(2))
  }

  //
  // Constructs a modified class body where the java declaration of the value()
  // method, where present, is replaced by a scala version.
  //
  // E.g. this java code body:
  //
  //  ... {
  //      String value() default "";
  //  }
  //
  // gets changed to this:
  //
  //  ... {
  //   def value(): String
  //  }
  //
  private def genNewBody(body: String): String = {
    val matcher =
      Pattern.compile("""(?m)^\s*(.*?) *value\(\).*;""").matcher(body)

    if (matcher.find()) {
      val valueType = matcher.group(1)

      val newValueType =
        valueType match {
          case "Class<? extends Suite>" => "Class[_ <: Suite]"
          case "String"                 => "String"
          case "String[]"               => "Array[String]"
          case _ =>
            throw new RuntimeException("unexpected valueType [" +
                                       valueType + "]")
      }

      val buf = new StringBuffer
      matcher.appendReplacement(buf, " def value(): "+ newValueType)
      matcher.appendTail(buf)

      buf.toString
    }
    else ""
  }

  def docJavaTags(javaSources: Set[File]): Set[File] = {
    def isAnnotation(fileContents: String): Boolean =
      fileContents.contains(
        "Note: This is actually an annotation defined in Java")

    for {
      srcFile <- javaSources
      val contents = Source.fromFile(srcFile).mkString
      if isAnnotation(contents)
    } yield {
      val filename = srcFile.getName
      val className = filename.replaceFirst("""\.java$""", "")
      val sep = File.separator
      val relativeDir =
        srcFile.getParent.replaceFirst(".*"+ sep +"main"+ sep +"java"+ sep, "")
      val destFile =
        file(docsrcDir + sep + relativeDir + sep + className +".scala")

      val (top, body) = parseContents(className, contents)

      val newBody = genNewBody(body)

      val newContents =
        top
          .replaceAll("""@Retention\(.*?\)""",  "")
          .replaceAll("""@Target\(.*?\)""",     "")
          .replaceAll("""@TagAnnotation.*\)""", "")
          .replaceAll("""@TagAnnotation""",     "")
          .replaceAll("""@Inherited""",         "")
          .replaceAll("""public *@interface""", "")
          .replaceAll("""(?m)^import.*$""",     "")
          .replaceAll(className + "$",
                      "trait "+ className +
                      " extends java.lang.annotation.Annotation "+ newBody +
                      "\n")

      if (!destFile.exists || (srcFile.lastModified > destFile.lastModified)) {
        createDirectory(file(destFile.getParent))

        val writer = new PrintWriter(destFile)
        try {
          writer.print(newContents)
        }
        finally {
          writer.close()
        }
      }
      destFile
    }
  }
}
