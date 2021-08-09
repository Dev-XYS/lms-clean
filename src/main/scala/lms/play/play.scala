package lms
package play

import lms.core.stub._
import lms.macros.SourceContext
import lms.core.virtualize
import lms.core.Graph

object Main {
  def main(args: Array[String]) {
    val snippet = new DslDriver[Int, Int] {
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
    println(snippet.g)
  }
}
