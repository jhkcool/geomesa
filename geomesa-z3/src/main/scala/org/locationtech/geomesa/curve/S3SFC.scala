/***********************************************************************
 * Copyright (c) 2013-2019 Commonwealth Computer Research, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Apache License, Version 2.0
 * which accompanies this distribution and is available at
 * http://www.opensource.org/licenses/apache2.0.php.
 ***********************************************************************/

package org.locationtech.geomesa.curve

import com.google.common.geometry._
import org.locationtech.geomesa.conf.CommonProperties
import org.locationtech.geomesa.curve.TimePeriod.TimePeriod

import scala.collection.JavaConverters

/**
  * @author sunyabo 2019年08月01日 10:21
  * @version V1.0
  */
class S3SFC(period: TimePeriod) extends S3SpaceFillingCurve[S2CellId] {

  protected val lonMin: Double = -180d
  protected val latMin: Double = -90d
  protected val lonMax: Double = 180d
  protected val latMax: Double = 90d

  var minLevel: Int = 0
  var maxLevel: Int = 30
  var levelMod: Int = 1
  var maxCells: Int = 8

  override def time: Int = BinnedTime.maxOffset(period).toInt

  val wholePeriod = Seq((0, time))

  override def index(x: Double, y: Double, lenient: Boolean): S2CellId = {
    try {
      require(x >= lonMin && x <= lonMax && y >= latMin && y <= latMax,
        s"Value(s) out of bounds ([$lonMin,$lonMax], [$latMin,$latMax]): $x, $y")
      S2CellId.fromLatLng(S2LatLng.fromDegrees(y, x))
    } catch {
      case _: IllegalArgumentException if lenient => lenientIndex(y, x)
    }
  }

  protected def lenientIndex(x: Double, y: Double): S2CellId = {
    val bx = if (x < lonMin) { lonMin } else if (x > lonMax) { lonMax } else { x }
    val by = if (y < latMin) { latMin } else if (y > latMax) { latMax } else { y }

    S2CellId.fromLatLng(S2LatLng.fromDegrees(by, bx))
  }

  /**
    * Gets google-s2 cell ids as ranges
    *
    * @return
    */
  override def ranges(xy: Seq[(Double, Double, Double, Double)],
                      maxRanges: Option[Int]): Seq[S2CellId] = {
    val startS2 = S2LatLng.fromDegrees(xy.head._2, xy.head._1)
    val endS2 = S2LatLng.fromDegrees(xy.head._4, xy.head._3)
    val rect = new S2LatLngRect(startS2, endS2)

    val coverer = new S2RegionCoverer
    coverer.setMinLevel(CommonProperties.S2MinLevel)
    coverer.setMaxLevel(CommonProperties.S2MaxLevel)
    coverer.setLevelMod(CommonProperties.S2LevelMod)
    coverer.setMaxCells(CommonProperties.S2MaxCells)

    val cellUnion = coverer.getCovering(rect)

    JavaConverters.asScalaIteratorConverter(cellUnion.cellIds().iterator).asScala.toSeq
  }
}

object S3SFC {

  private val SfcDay   = new S3SFC(TimePeriod.Day)
  private val SfcWeek  = new S3SFC(TimePeriod.Week)
  private val SfcMonth = new S3SFC(TimePeriod.Month)
  private val SfcYear  = new S3SFC(TimePeriod.Year)

  def apply(period: TimePeriod): S3SFC = period match {
    case TimePeriod.Day   => SfcDay
    case TimePeriod.Week  => SfcWeek
    case TimePeriod.Month => SfcMonth
    case TimePeriod.Year  => SfcYear
  }
}
