package com.hikvision.geomesa.index.index

import java.time.ZonedDateTime

import org.locationtech.geomesa.curve.S3SFC
import org.locationtech.geomesa.filter.{Bounds, FilterValues}
import org.locationtech.jts.geom.Geometry

/**
  * @author sunyabo 2019年08月01日 09:25
  * @version V1.0
  */
package object s3 {

  case class S3IndexKey(bin: Short, s: Long, offset: Int) extends Ordered[S3IndexKey] {
    override def compare(that: S3IndexKey): Int = {
      val b = Ordering.Short.compare(bin, that.bin)
      if (b != 0) { b } else {
        Ordering.Long.compare(s, that.s)
      }
    }
  }

  case class S3IndexValues(sfc: S3SFC,
                           geometries: FilterValues[Geometry],
                           spatialBounds: Seq[(Double, Double, Double, Double)],
                           intervals: FilterValues[Bounds[ZonedDateTime]],
                           temporalBounds: Map[Short, Seq[(Int, Int)]],
                           temporalUnbounded: Seq[(Short, Short)])
}
