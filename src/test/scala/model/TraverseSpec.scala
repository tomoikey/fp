package model

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import Traverse._

class TraverseSpec extends Properties("Traverse") {
  property("compose and map") = forAll {
    (complexMap: Map[String, List[Option[Int]]]) =>
      val traverse =
        mapTraverse[String].compose(listTraverse).compose(optionTraverse)
      val actual = traverse.map(complexMap)(_ * 2)
      val expected = complexMap.map { case (k, v) => (k, v.map(_.map(_ * 2))) }
      actual == expected
  }
}
