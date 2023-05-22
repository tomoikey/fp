object Main {
  def main(args: Array[String]): Unit = {
    sealed class Hoge() {
      def update(a: String, b: String => String): Unit = {
        println(s"a: ${a}")
        println(s"result: ${b(a)}")
      }
    }
    lazy val aaa = new Hoge()
    aaa("a") = (s: String) => s
  }
}
