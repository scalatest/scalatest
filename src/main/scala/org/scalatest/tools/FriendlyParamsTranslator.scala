package org.scalatest.tools

import collection.mutable.ListBuffer

private[scalatest] class FriendlyParamsTranslator {
  
  private[scalatest] val validConfigMap = Map(
                                             "dropteststarting" -> "N", 
                                             "droptestsucceeded" -> "C", 
                                             "droptestignored" -> "X", 
                                             "droptestpending" -> "E", 
                                             "dropsuitestarting" -> "H", 
                                             "dropsuitecompleted" -> "L", 
                                             "dropinfoprovided" -> "O", 
                                             "nocolor" -> "W", 
                                             "shortstacks" -> "S", 
                                             "fullstacks" -> "F", 
                                             "durations" -> "D"
                                           )
  
  private [scalatest] def extractContentInBracket(raw:String, it:Iterator[String], expected:String):String = {
    if(!raw.startsWith("("))
      throw new IllegalArgumentException("Invalid configuration, example valid configuration: " + expected)
    val withBrackets = if(raw.endsWith(")"))
                         raw
                       else 
                         parseUntilFound(raw, ")", it)
    withBrackets.substring(1, withBrackets.length() - 1)
  }
    
  private[scalatest] def parseUntilFound(value:String, endsWith:String, it:Iterator[String]):String = {
    if(it.hasNext) {
      val next = it.next()
      if(next.endsWith(endsWith))
        value + next
      else
        parseUntilFound(value + next, endsWith, it)
    }
    else
      throw new IllegalArgumentException("Unable to find '" + endsWith + "'")
  }
    
  private[scalatest] def parseCompoundParams(rawParamsStr:String, it:Iterator[String], expected:String):Array[String] = {
    val rawClassArr = extractContentInBracket(rawParamsStr, it, expected).split(",")
    for(rawClass <- rawClassArr) yield {
      val trimmed = rawClass.trim()
      if(trimmed.length() > 1 && trimmed.startsWith("\"") && trimmed.endsWith("\""))
        trimmed.substring(1, trimmed.length() - 1)
      else
        trimmed
    }
  }
    
  private[scalatest] def translateCompoundParams(rawParamsStr:String, it:Iterator[String], expected:String):String = {
    val paramsArr = parseCompoundParams(rawParamsStr, it, expected)
    paramsArr.mkString(" ")
  }
    
  private[scalatest] def parseParams(rawParamsStr:String, it:Iterator[String], validParamSet:Set[String], expected:String):Map[String, String] = {    
    if(rawParamsStr.length() > 0) {
      val paramsStr = extractContentInBracket(rawParamsStr, it, expected)
      val configsArr:Array[String] = paramsStr.split(",")
      val tuples = for(configStr <- configsArr) yield {
        val keyValueArr = configStr.trim().split("=")
        if(keyValueArr.length == 2) {
          // Value config param
          val key:String = keyValueArr(0).trim()
          if(!validParamSet.contains(key))
            throw new IllegalArgumentException("Invalid configuration: " + key)
          val rawValue = keyValueArr(1).trim()
          val value:String = 
            if(rawValue.startsWith("\"") && rawValue.endsWith("\"") && rawValue.length() > 1) 
              rawValue.substring(1, rawValue.length() - 1)
            else
              rawValue
          (key -> value)
        }
        else
          throw new IllegalArgumentException("Invalid configuration: " + configStr)
      }
      Map[String, String]() ++ tuples
    }
    else
      Map[String, String]()
  }
    
  private[scalatest] def translateConfigs(rawConfigs:String):String = {
    val configArr = rawConfigs.split(" ")
    val translatedArr = configArr.map {config => 
          val translatedOpt:Option[String] = validConfigMap.get(config)
          translatedOpt match {
            case Some(translated) => translated
            case None => throw new IllegalArgumentException("Invalid config value: " + config)
          }
        }
    translatedArr.mkString
  }
    
  private[scalatest] def getTranslatedConfig(paramsMap:Map[String, String]):String = {
    val configOpt:Option[String] = paramsMap.get("config")
	configOpt match {
	  case Some(configStr) => translateConfigs(configStr)
	  case None => ""
	}
  }
  
  // Template method as extension point for subclasses.
  private[scalatest] def validateSupportedPropsAndTags(s:String) {
    
  }
  
  private[scalatest] def translateCompound(inputString:String, friendlyName:String, dash:String, it:Iterator[String]):List[String] = {
    val translatedList = new ListBuffer[String]()
    val elements:Array[String] = parseCompoundParams(inputString.substring(friendlyName.length()), it, friendlyName + "(a, b, c)")
    elements.foreach{ element => 
      translatedList += dash
      translatedList += element
    }
    translatedList.toList
  }
  
  private[scalatest] def parseDashAndArgument(dash:String, replaceDeprecated:String, it:Iterator[String]):List[String] = {
    // Do not show deprecated message for now, until the friendly dsl is really ready
    //println(dash + " is deprecated, use " + replaceDeprecated + " instead.")
    val translatedList = new ListBuffer[String]()
    translatedList += dash
    if(it.hasNext)
          translatedList += it.next
    translatedList.toList
  }
  
  private[scalatest] def showDeprecated(inputString:String, replaceDeprecated:String):String = {
    // May be we can use a logger later
    // Do not show deprecated message for now, until the friendly dsl is really ready
    //println(inputString + " is deprecated, use " + replaceDeprecated + " instead.")
    inputString
  }
  
  private[scalatest] def translateKeyValue(value:String, elementName:String, translated:String, requiredAttrList:List[String], 
                                          optionalAttrList:List[String], exampleValid:String, it:Iterator[String]):List[String] = {
    val paramsMap:Map[String, String] = parseParams(value.substring(elementName.length()), it, (requiredAttrList ::: optionalAttrList).toSet, exampleValid)
    val translatedList = new ListBuffer[String]()
    translatedList += translated + getTranslatedConfig(paramsMap)
    requiredAttrList.filter(attr => attr != "config").foreach { attr => 
      val option:Option[String] = paramsMap.get(attr)
      option match {
        case Some(value) => translatedList += value
        case None => throw new IllegalArgumentException(elementName + " requires " + attr + " to be specified, example: " + exampleValid)
      }
    }
    optionalAttrList.filter(attr => attr != "config").foreach { attr => 
      val option:Option[String] = paramsMap.get(attr)
      option match {
        case Some(value) => translatedList += value
        case None => // Do nothing since it's optional
      }
    }
    translatedList.toList
  }

  private[scalatest] def parsePropsAndTags(args: Array[String]) = {

    val props = new ListBuffer[String]()
    val includes = new ListBuffer[String]()
    val excludes = new ListBuffer[String]()
    val repoArgs = new ListBuffer[String]()
    val concurrent = new ListBuffer[String]()
    val memberOnlys = new ListBuffer[String]()
    val wildcards = new ListBuffer[String]()
    val suites = new ListBuffer[String]()
    val junits = new ListBuffer[String]()
    val testngs = new ListBuffer[String]()

    val it = args.iterator
    while (it.hasNext) {

      val s = it.next

      validateSupportedPropsAndTags(s)
      
      if (s.startsWith("-D")) 
        props += s
      else if (s =="-n") 
        includes ++= parseDashAndArgument(s, "include", it)
      else if (s.startsWith("include")) 
        includes ++= List("-n", translateCompoundParams(s.substring("include".length()), it, "include(a, b, c)"))
      else if (s == "-l") 
        excludes ++= parseDashAndArgument(s, "exclude", it)
      else if (s.startsWith("exclude")) 
        excludes ++= List("-l", translateCompoundParams(s.substring("exclude".length()), it, "exclude(a, b, c)"))
	  else if (s.startsWith("-o")) 
        repoArgs += showDeprecated(s, "stdout")
      else if (s.startsWith("stdout")) 
        repoArgs += "-o" + getTranslatedConfig(parseParams(s.substring("stdout".length()), it, Set("config"), "stdout"))
      else if (s.startsWith("-e")) 
        repoArgs += showDeprecated(s, "stderr")
      else if (s.startsWith("stderr")) 
        repoArgs += "-e" + getTranslatedConfig(parseParams(s.substring("stderr".length()), it, Set("config"), "stderr"))
      else if (s.startsWith("-g")) 
        repoArgs += showDeprecated(s, "graphic")
      else if (s.startsWith("graphic")) {
        val paramsMap:Map[String, String] = parseParams(s.substring("graphic".length()), it, Set("config"), "graphic")
        val dashG = "-g" + getTranslatedConfig(paramsMap)
        if(dashG.indexOf("S") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'shortstacks' (present short stack traces) configuration parameter for the graphic reporter (because it shows them anyway): ")
        if(dashG.indexOf("F") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'fullstacks' (present full stack traces) configuration parameter for the graphic reporter (because it shows them anyway): ")
        if(dashG.indexOf("W") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'nocolor' (present without color) configuration parameter for the graphic reporter")
        if(dashG.indexOf("D") >= 0 )
          throw new IllegalArgumentException("Cannot specify an 'durations' (present all durations) configuration parameter for the graphic reporter (because it shows them all anyway)")
        repoArgs += dashG
      }
      else if (s.startsWith("-f")) 
        repoArgs ++= parseDashAndArgument(s, "file(directory=\"xxx\")", it)
      else if (s.startsWith("file")) 
        repoArgs ++= translateKeyValue(s, "file", "-f", List("filename"), List("config"), "file(directory=\"xxx\")", it)
      else if (s.startsWith("-u")) 
        repoArgs ++= parseDashAndArgument(s, "junitxml(directory=\"xxx\")", it)
      else if(s.startsWith("junitxml")) 
        repoArgs ++= translateKeyValue(s, "junitxml", "-u", List("directory"), Nil, "junitxml(directory=\"xxx\")", it)
      // To be enabled when and if RunCompleted, RunAborted and RunStopped can be supported correctly in ScalaTestFramework
      /*else if (s.startsWith("-d")) 
        repoArgs ++= parseDashAndArgument(s, "dashboard(directory=\"xxx\", archive=\"xxx\")", it)
      else if (s.startsWith("-a")) 
        repoArgs ++= parseDashAndArgument(s, "dashboard(directory=\"xxx\", archive=\"xxx\")", it)
      else if (s.startsWith("dashboard")) {
        repoArgs += "-d"
        val paramsMap:Map[String, String] = parseParams(s.substring("dashboard".length()), it, Set("directory", "archive"), "dashboard(directory=\"xxx\", archive=\"xxx\")")
        val directoryOpt:Option[String] = paramsMap.get("directory")
        directoryOpt match {
          case Some(dir) => repoArgs += dir
          case None => throw new IllegalArgumentException("dashboard requires directory to be specified, example: dashboard(directory=\"xxx\", archive=\"xxx\")")
        }
        val archiveOpt:Option[String] = paramsMap.get("archive")
        archiveOpt match {
          case Some(archive) => 
            if (archive.length == 0)
              throw new IllegalArgumentException("dashboard archive value cannot be empty string, example: dashboard(directory=\"xxx\", archive=\"xxx\")")
            repoArgs += "-a"
            repoArgs += archive
          case None => 
        }
      }*/
      // To be enabled when and if native scalatest xml reporter is available
      /*else if (s.startsWith("-x")) 
        repoArgs ++= parseDashAndArgument(s, "xml(directory=\"xxx\")", it)
      else if (s.startsWith("xml")) 
        repoArgs ++= translateKeyValue(s, "xml", "-x", List("directory"), Nil, "xml(directory=\"xxx\")", it)*/
      else if (s.startsWith("-h")) 
        repoArgs ++= parseDashAndArgument(s, "html(directory=\"xxx\", css=\"xxx\")", it)
      else if (s.startsWith("-Y")) 
        repoArgs ++= parseDashAndArgument(s, "html(directory=\"xxx\", css=\"xxx\")", it)
      else if (s.startsWith("html")) {
        repoArgs += "-h"
        val paramsMap:Map[String, String] = parseParams(s.substring("html".length), it, Set("directory", "css"), "html(directory=\"xxx\", css=\"xxx\")")
        val directoryOpt:Option[String] = paramsMap.get("directory")
        directoryOpt match {
          case Some(dir) => repoArgs += dir
          case None => throw new IllegalArgumentException("html requires directory to be specified, example: html(directory=\"xxx\", css=\"xxx\")")
        }
        val cssOpt:Option[String] = paramsMap.get("css")
        cssOpt match {
          case Some(css) => 
            if (css.length == 0)
              throw new IllegalArgumentException("html's css value cannot be empty string, example: html(directory=\"xxx\", css=\"xxx\")")
            repoArgs += "-Y"
            repoArgs += css
          case None => 
        }
      }
      else if (s.startsWith("-r")) 
        repoArgs ++= parseDashAndArgument(s, "reporterclass(classname=\"xxx\")", it)
      else if (s.startsWith("reporterclass")) {
        val paramsMap:Map[String, String] = parseParams(s.substring("reporterclass".length()), it, Set("classname", "config"), "reporterclass(classname=\"xxx\")")
        val classnameOpt:Option[String] = paramsMap.get("classname")
        val classname:String = classnameOpt match {
          case Some(clazzname) => clazzname
          case None => throw new IllegalArgumentException("reporterclass requires classname to be specified, example: reporterclass(classname=\"xxx\")")
        }
        val dashR = "-r" + getTranslatedConfig(paramsMap)
        if(dashR.indexOf("S") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'shortstacks' (present short stack traces) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("F") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'fullstacks' (present full stack traces) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("W") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'nocolor' (present without color) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("D") >= 0 )
          throw new IllegalArgumentException("Cannot specify an 'durations' (present all durations) configuration parameter for a custom reporter: " + dashR + " " + classname)
        repoArgs += dashR
        repoArgs += classname
      }
      else if(s == "-c" || s == "concurrent") 
        concurrent += "-c"
      else if(s == "-m") 
        memberOnlys ++= parseDashAndArgument(s, "membersonly(a, b, c)", it)
      else if(s.startsWith("membersonly")) 
        memberOnlys ++= translateCompound(s, "membersonly", "-m", it)
      else if(s == "-w") 
        wildcards ++= parseDashAndArgument(s, "wildcard(a, b, c)", it)
      else if(s.startsWith("wildcard")) 
        wildcards ++= translateCompound(s, "wildcard", "-w", it)
      else if(s == "-s") 
        suites ++= parseDashAndArgument(s, "suite(a, b, c)", it)
      else if(s.startsWith("suite")) 
        suites ++= translateCompound(s, "suite", "-s", it)
      else if(s == "-j") 
        junits ++= parseDashAndArgument(s, "junit(a, b, c)", it)
      else if(s.startsWith("junit")) 
        junits ++= translateCompound(s, "junit", "-j", it)
      else if(s == "-t")
        testngs ++= parseDashAndArgument(s, "testng(a, b, c)", it)
      else if(s.startsWith("testng")) 
        testngs ++= translateCompound(s, "testng", "-t", it)
      else
          throw new IllegalArgumentException("Unrecognized argument: " + s)
    }
    (props.toList, includes.toList, excludes.toList, repoArgs.toList, concurrent.toList, memberOnlys.toList, wildcards.toList, 
     suites.toList, junits.toList, testngs.toList)
  }

}
