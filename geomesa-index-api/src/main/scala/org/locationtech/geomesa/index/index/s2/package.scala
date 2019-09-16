
package com.hikvision.geomesa.index.index

import org.locationtech.geomesa.curve.S2SFC
import org.locationtech.geomesa.filter.FilterValues
import org.locationtech.jts.geom.Geometry

/**
  * <p></p>
  *
  * @author sunyabo 2019年07月24日 14:26
  * @version V1.0
  */
package object s2 {

  case class S2IndexValues(sfc: S2SFC,
                           geometries: FilterValues[Geometry],
                           bounds: Seq[(Double, Double, Double, Double)])
}
