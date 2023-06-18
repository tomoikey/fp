import model.{Free, Functor, Monad}
import model.Free.*
import model.Functor.given_Functor_Option

import util.chaining.scalaUtilChainingOps
object Main {
  def main(args: Array[String]): Unit = {
    val option3 = Option(Option(Option(1)))

//    val optionOptionHKFolder: Option[Option[FreeM[Option, Int]]] =
//      option3.map(_.map(n => Free(n.map(Pure(_): FreeM[Option, Int]))))
//    val optionHKFolder: Option[FreeM[Option, Int]] =
//      optionOptionHKFolder.map(Free(_): FreeM[Option, Int])
//    val hkFolder: FreeM[Option, Int] = Free(optionHKFolder)

//    val aaa: FreeM[Option, Int] = for {
//      a <- for {
//        a <- Pure[Option, Int](100)
//        optionInt = Some(200): Option[Int]
//        b <- Pure(a + 100)
//        c <- Free(optionInt.map(Pure(_)))
//      } yield b + c
//      b <- Pure(a + 100)
//    } yield b
//
//    aaa.tap(println)
  }
}
