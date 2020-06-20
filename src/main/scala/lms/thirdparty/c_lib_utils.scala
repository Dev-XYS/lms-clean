package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._


trait CMacro { b: Base =>
  def cmacro[T:Manifest](m:String): Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("macro", lms.core.Backend.Const(m)))
  }
}
trait CCodeGenCMacro extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(_, "macro", List(Const(m:String)), _) =>
      emit(m)
    case _ => super.shallow(n)
  }
}

trait LibStruct { b : Base =>
  def libStruct[T:Manifest](m:String):Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("lib-struct", lms.core.Backend.Const(m)))
  }
  // T: type of struct . U: type of field
  def readField[T:Manifest, U:Manifest](x: Rep[T], m:String): Rep[U] = {
    Wrap[U](Adapter.g.reflectRead("read-field", Unwrap(x), lms.core.Backend.Const(m))(Unwrap(x)))
  }
}
trait CCodeGenLibStruct extends ExtendedCCodeGen {
  override def mayInline(n: Node): Boolean = n match {
    case Node(_, "lib-struct", _, _) => false
    case _ => super.mayInline(n)
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "read-field", List(x: Sym, Const(m: String)), _) =>
      shallow(x); emit(s".$m")
    case _ => super.shallow(n)
  }

  override def traverse(n: Node): Unit = n match {
    case Node(s, "lib-struct", List(Const(m: String)), _) =>
      emit(s"$m "); shallow(s); emitln(";")
    case _ => super.traverse(n)
  }
}
