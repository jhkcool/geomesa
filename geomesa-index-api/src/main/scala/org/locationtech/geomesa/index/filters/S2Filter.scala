package org.locationtech.geomesa.index.filters

import java.nio.ByteBuffer

import com.google.common.geometry.S2CellId
import com.hikvision.geomesa.index.index.s2.S2IndexValues
import org.locationtech.geomesa.utils.index.ByteArrays

/**
  * @author sunyabo 2019年07月29日 08:29
  * @version V1.0
  */
class S2Filter(val xy: Array[Array[Double]]) {
  def inBounds(buf: Array[Byte], offset: Int): Boolean = {
    val s2CellId = new S2CellId(ByteArrays.readLong(buf, offset))
    val x = s2CellId.toLatLng.lngDegrees()
    val y = s2CellId.toLatLng.latDegrees()
    var i = 0
    while (i < xy.length) {
      val xyi = xy(i)
      if (x >= xyi(0) && x <= xyi(2) && y >= xyi(1) && y <= xyi(3)) {
        return true
      }
      i += 1
    }
    false
  }
}

object S2Filter {

  private val RangeSeparator = ":"
  private val TermSeparator  = ";"

  val XYKey     = "sxy"
  val TKey      = "st"
  val EpochKey  = "epoch"

  def apply(values: S2IndexValues): S2Filter = {
    val sfc = values.sfc
    val xy: Array[Array[Double]] = values.bounds.map { case (xmin, ymin, xmax, ymax) =>
      Array(xmin, ymin, xmax, ymax)
    }.toArray

    new S2Filter(xy)
  }

  def serializeToBytes(filter: S2Filter): Array[Byte] = {
    // 4 bytes for length plus 16 bytes for each xy val (4 doubles)
    val xyLength = 4 + filter.xy.length * 32
    val buffer = ByteBuffer.allocate(xyLength)

    buffer.putInt(filter.xy.length)

    filter.xy.foreach(bounds => bounds.foreach(buffer.putDouble))

    buffer.array()
  }

  def deserializeFromBytes(serialized: Array[Byte]): S2Filter = {
    val buffer = ByteBuffer.wrap(serialized)
    val xy = Array.fill(buffer.getInt())(Array.fill(4)(buffer.getDouble))
    new S2Filter(xy)
  }

  def serializeToStrings(filter: S2Filter): Map[String, String] = {
    val xy = filter.xy.map(bounds => bounds.mkString(RangeSeparator)).mkString(TermSeparator)
    Map(XYKey -> xy)
  }

  def deserializeFromStrings(serialized: scala.collection.Map[String, String]): S2Filter = {
    val xy = serialized(XYKey).split(TermSeparator).map(_.split(RangeSeparator).map(_.toDouble))
    new S2Filter(xy)
  }
}
