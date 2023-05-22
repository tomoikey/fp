package model

import model.Tree._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class TreeSpec extends Properties("Tree") {
  property("map") = forAll { (time: Int, a: Int, b: Int, c: Int) =>
    val base = Branch(Leaf(a), Branch(Leaf(b), Leaf(c)))
    val actual = base.map(_ * time)
    val expect = Branch(Leaf(time * a), Branch(Leaf(time * b), Leaf(time * c)))
    actual == expect
  }
}
