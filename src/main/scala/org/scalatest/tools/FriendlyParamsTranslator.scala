package org.scalatest.tools

import collection.mutable.ListBuffer

private[scalatest] object FriendlyParamsTranslator {
  
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

  private[scalatest] def translateArguments(args: Array[String]): Array[String] = {

    val newArgs = new ListBuffer[String]
    val it = args.iterator
    while (it.hasNext) {

      val s = it.next
      if (s.startsWith("include")) {
        println("WARNING: Argument 'include' has been deprecated and will be removed in a future version of ScalaTest.  Please use -n instead.")
        newArgs ++= List("-n", translateCompoundParams(s.substring("include".length()), it, "include(a, b, c)"))
      }
      else if (s.startsWith("exclude")) { 
        println("WARNING: Argument 'exclude' has been deprecated and will be removed in a future version of ScalaTest.  Please use -l instead.")
        newArgs ++= List("-l", translateCompoundParams(s.substring("exclude".length()), it, "exclude(a, b, c)"))
      }
      else if (s.startsWith("stdout")) {
        println("WARNING: Argument 'stdout' has been deprecated and will be removed in a future version of ScalaTest.  Please use -o instead.")
        newArgs += "-o" + getTranslatedConfig(parseParams(s.substring("stdout".length()), it, Set("config"), "stdout"))
      }
      else if (s.startsWith("stderr")) {
        println("WARNING: Argument 'stderr' has been deprecated and will be removed in a future version of ScalaTest.  Please use -e instead.")
        newArgs += "-e" + getTranslatedConfig(parseParams(s.substring("stderr".length()), it, Set("config"), "stderr"))
      }
      /*else if (s.startsWith("graphic")) {
        println("WARNING: Argument 'graphic' has been deprecated and will be removed in a future version of ScalaTest.  Please use -g instead.")
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
        newArgs += dashG
      }*/
      else if (s.startsWith("file")) {
        println("WARNING: Argument 'file' has been deprecated and will be removed in a future version of ScalaTest.  Please use -f instead.")
        newArgs ++= translateKeyValue(s, "file", "-f", List("filename"), List("config"), "file(directory=\"xxx\")", it)
      }
      else if(s.startsWith("junitxml")) {
        println("WARNING: Argument 'junitxml' has been deprecated and will be removed in a future version of ScalaTest.  Please use -u instead.")
        newArgs ++= translateKeyValue(s, "junitxml", "-u", List("directory"), Nil, "junitxml(directory=\"xxx\")", it)
      }
      else if (s.startsWith("html")) {
        println("WARNING: Argument 'html' has been deprecated and will be removed in a future version of ScalaTest.  Please use -h instead.")
        newArgs += "-h"
        val paramsMap:Map[String, String] = parseParams(s.substring("html".length), it, Set("directory", "css"), "html(directory=\"xxx\", css=\"xxx\")")
        val directoryOpt:Option[String] = paramsMap.get("directory")
        directoryOpt match {
          case Some(dir) => newArgs += dir
          case None => throw new IllegalArgumentException("html requires directory to be specified, example: html(directory=\"xxx\", css=\"xxx\")")
        }
        val cssOpt:Option[String] = paramsMap.get("css")
        cssOpt match {
          case Some(css) => 
            if (css.length == 0)
              throw new IllegalArgumentException("html's css value cannot be empty string, example: html(directory=\"xxx\", css=\"xxx\")")
            newArgs += "-Y"
            newArgs += css
          case None => 
        }
      }
      else if (s.startsWith("reporterclass")) {
        println("WARNING: Argument 'reporterclass' has been deprecated and will be removed in a future version of ScalaTest.  Please use -g instead.")
        val paramsMap:Map[String, String] = parseParams(s.substring("reporterclass".length()), it, Set("classname", "config"), "reporterclass(classname=\"xxx\")")
        val classnameOpt:Option[String] = paramsMap.get("classname")
        val classname:String = classnameOpt match {
          case Some(clazzname) => clazzname
          case None => throw new IllegalArgumentException("reporterclass requires classname to be specified, example: reporterclass(classname=\"xxx\")")
        }
        val dashR = "-C" + getTranslatedConfig(paramsMap)
        if(dashR.indexOf("S") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'shortstacks' (present short stack traces) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("F") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'fullstacks' (present full stack traces) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("W") >= 0)
          throw new IllegalArgumentException("Cannot specify an 'nocolor' (present without color) configuration parameter for a custom reporter: " + dashR + " " + classname)
        if(dashR.indexOf("D") >= 0 )
          throw new IllegalArgumentException("Cannot specify an 'durations' (present all durations) configuration parameter for a custom reporter: " + dashR + " " + classname)
        newArgs += dashR
        newArgs += classname
      }
      else if(s.startsWith("membersonly")) 
        newArgs ++= translateCompound(s, "membersonly", "-m", it)
      else if(s.startsWith("wildcard")) 
        newArgs ++= translateCompound(s, "wildcard", "-w", it)
      else
        newArgs += s
    }
    
    newArgs.toArray
  }

}
