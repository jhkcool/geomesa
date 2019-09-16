package org.locationtech.geomesa.curve

import com.google.common.geometry._
import org.locationtech.geomesa.conf.CommonProperties

import scala.collection.JavaConversions._

/**
  * s2 space-filling curve
  *
  * @author sunyabo 2019年07月26日 09:11
  * @version V1.0
  */
class S2SFC extends S2SpaceFillingCurve[S2CellId] {

  private val lonMin: Double = -180d
  private val latMin: Double = -90d
  private val lonMax: Double = 180d
  private val latMax: Double = 90d

  override def index(x: Double, y: Double, lenient: Boolean): S2CellId = {
    try {
      require(x >= lonMin && x <= lonMax && y >= latMin && y <= latMax,
        s"Value(s) out of bounds ([$lonMin,$lonMax], [$latMin,$latMax]): $x, $y")
      S2CellId.fromLatLng(S2LatLng.fromDegrees(y, x))
    } catch {
      case _: IllegalArgumentException if lenient => lenientIndex(x, y)
    }
  }

  protected def lenientIndex(x: Double, y: Double): S2CellId = {
    val bx = if (x < lonMin) { lonMin } else if (x > lonMax) { lonMax } else { x }
    val by = if (y < latMin) { latMin } else if (y > latMax) { latMax } else { y }
    S2CellId.fromLatLng(S2LatLng.fromDegrees(by, bx))
  }

  /**
    * get s2 cell union as ranges
    * @return
    */
  override def ranges(xy: Seq [(Double, Double, Double, Double)],
                      maxRanges: Option[Int]): Seq[S2CellId] = {

    val rect = new S2LatLngRect(S2LatLng.fromDegrees(xy.head._2, xy.head._1),
      S2LatLng.fromDegrees(xy.head._4, xy.head._3))

    val cover: S2RegionCoverer = new S2RegionCoverer
    cover.setMinLevel(CommonProperties.S2MinLevel)
    cover.setMaxLevel(CommonProperties.S2MaxLevel)
    cover.setLevelMod(CommonProperties.S2LevelMod)
    cover.setMaxCells(CommonProperties.S2MaxCells)

    val s2CellUnion = cover.getCovering(rect)

    s2CellUnion.cellIds().toSeq
  }
}

object S2SFC {
  val s2SFC = new S2SFC
}
