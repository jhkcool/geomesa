/***********************************************************************
 * Copyright (c) 2013-2019 Commonwealth Computer Research, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Apache License, Version 2.0
 * which accompanies this distribution and is available at
 * http://www.opensource.org/licenses/apache2.0.php.
 ***********************************************************************/

package org.locationtech.geomesa.conf

import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}


/**
  * <p></p>
  *
  * @author jianghongkang 2019年05月10日 20:11
  * @version V1.0
  */
object CommonProperties extends LazyLogging {

  val S2CoverConfig = SystemProperty("google.s2.coverer.config", "0,30,1,8")
    .get.trim.split(",")
    .map(item => item.toInt)
  val S2MinLevel = S2CoverConfig(0)
  val S2MaxLevel = S2CoverConfig(1)
  val S2LevelMod = S2CoverConfig(2)
  val S2MaxCells = S2CoverConfig(3)


  case class SystemProperty(property: String, default: String = null) {

    val threadLocalValue = new ThreadLocal[String]()

    def set(value: String): Unit = System.setProperty(property, value)

    def clear(): Unit = System.clearProperty(property)

    def get: String = ConfigLoader.Config.get(property) match {
      case Some((value, true))  => value // final value - can't be overridden
      case Some((value, false)) => fromSysProps.getOrElse(value)
      case None => fromSysProps.getOrElse(default)
    }

    def option: Option[String] = Option(get)

    def toInt: Option[Int] = option.flatMap { value =>
      Try(value.toInt) match {
        case Success(v) => Some(v)
        case Failure(_) =>
          logger.warn(s"Invalid integer for property $property: $value")
          Option(default).map(_.toInt)
      }
    }

    def toBoolean: Option[Boolean] = option.flatMap { value =>
      Try(value.toBoolean) match {
        case Success(v) => Some(v)
        case Failure(_) =>
          logger.warn(s"Invalid Boolean for property $property: $value")
          Option(default).map(java.lang.Boolean.parseBoolean)
      }
    }

    private def fromSysProps: Option[String] =
      Option(threadLocalValue.get).orElse(sys.props.get(property).filterNot(_.isEmpty))
  }

  // For dynamic properties that are not in geomesa-site.xml.template, this is intended
  // to be a System.getProperty drop-in replacement that ensures the config is always loaded.
  def getProperty(prop: String): String = SystemProperty(prop).get
}
