package lms
package play

import lms.core.stub._
import lms.macros.SourceContext
import lms.core.virtualize
import lms.core.Graph

object Main {
  def main(args: Array[String]) {
    lazy val snippet = new DslDriver[Int, Int] {
      @virtualize
      def snippet(x: Rep[Int]) = {
        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true) + compute(1 == 1)
      }

      val g: Graph = Adapter.genGraph1(manifest[Int], manifest[Int])(x =>
        Unwrap(wrapper(Wrap[Int](x)))
      )
    }

    lazy val snippet_power = new DslDriver[Int, Int] {
      def square(x: Rep[Int]): Rep[Int] = x * x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n / 2))
        else b * power(b, n - 1)

      def snippet(b: Rep[Int]) =
        power(b, 7)

      val g: Graph = Adapter.genGraph1(manifest[Int], manifest[Int])(x =>
        Unwrap(wrapper(Wrap[Int](x)))
      )
    }

    lazy val driver = new DslDriverC[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = NewArray[Int](10)
        val arr2 = arr.slice(0, 5)
        arr2(0) = 5
        printf("%d %d", arr(0), arr(1))
      }

      val g: Graph = Adapter.genGraph1(manifest[Int], manifest[Int])(x =>
        Unwrap(wrapper(Wrap[Int](x)))
      )
    }

    println(driver.g)
  }
}
