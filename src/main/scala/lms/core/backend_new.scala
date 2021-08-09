package lms.core

import scala.collection.{immutable, mutable}

object Backend {
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {}
  case class Const(x: Any) extends Exp {}

  implicit val orderingExp: Ordering[Exp] = Ordering.by(e =>
    e match {
      case Const(s) => -s.##.abs
      case Sym(n)   => n
    }
  )
  implicit val orderingSym: Ordering[Sym] = Ordering.by(_.n)

  case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary) {}

  abstract class Def

  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary)
      extends Def {
    def bound: List[Sym] = List()
    def used: Set[Sym] = Set()
    def deps: Set[Sym] = Set()
    def isPure = true
  }

  final def UNSAFE = Const(0)

  case class EffectSummary(
      sdeps: Set[Sym],
      hdeps: Set[Sym],
      rkeys: Set[Exp],
      wkeys: Set[Exp]
  ) {
    lazy val keys: Set[Exp] = Set()
    lazy val deps: Set[Sym] = Set()
    def repr(f: Exp => String) = ""
    def isEmpty = isPure
    lazy val isPure = true
    lazy val hasSimpleEffect = false
  }

  def blocks(x: Node): List[Block] = List()
  def directSyms(x: Node): List[Sym] = List()
  def syms(x: Node): Set[Sym] = Set()
  def hardSyms(x: Node): Set[Sym] = Set()
  def boundSyms(x: Node): Seq[Sym] = Seq()

  final val emptySummary = EffectSummary(Set(), Set(), Set(), Set())
  final def softSummary(s: Sym) = EffectSummary(Set(s), Set(), Set(), Set())
  final def hardSummary(s: Sym) = EffectSummary(Set(), Set(s), Set(), Set())
  final def writeSummary(s: Exp) = EffectSummary(Set(), Set(), Set(), Set(s))
}

import Backend._

class GraphBuilder {
  val dummySym = Sym(0)
  val dummyEffect = EffectSummary(Set(), Set(), Set(), Set())
  val dummyBlock = Block(List(), dummySym, dummySym, dummyEffect)

  val globalDefs = new mutable.ArrayBuffer[Node]
  val globalDefsCache = new mutable.HashMap[Sym, Node]
  val globalDefsReverseCache = new mutable.HashMap[(String, Seq[Def]), Node]

  def fresh = 0

  object Def {
    def unapply(xs: Def): Option[(String, List[Def])] = None
  }

  def reflect(s: String, as: Def*): Exp = dummySym
  def reflect(x: Sym, s: String, as: Def*)(
      summary: EffectSummary = emptySummary
  ): Sym = dummySym
  def reflectRead(s: String, as: Def*)(efKeys: Exp*) = dummySym
  def reflectWrite(s: String, as: Def*)(efKeys: Exp*) = dummySym
  def reflectMutable(s: String, as: Def*) = dummySym
  def reflectFree(s: String, as: Def*)(efKeys: Exp) = dummySym
  def reflectRealloc(s: String, as: Def*)(efKeys: Exp) = dummySym
  def reflectUnsafe(s: String, as: Def*) = dummySym
  def reflectEffect(s: String, as: Def*)(readEfKeys: Exp*)(
      writeEfKeys: Exp*
  ): Exp = dummySym
  def reflectEffectSummary(s: String, as: Def*)(
      efKeys: (Set[Exp], Set[Exp])
  ): Exp = dummySym
  def reflectEffectSummaryHere(s: String, as: Def*)(
      efKeys: (Set[Exp], Set[Exp])
  ): Exp = dummySym

  def reify(f: => Exp): Block = dummyBlock
  def reifyHere(f: => Exp): Block = dummyBlock
  def reify(f: Exp => Exp): Block = dummyBlock
  def reifyHere(f: Exp => Exp): Block = dummyBlock
  def reify(f: (Exp, Exp) => Exp): Block = dummyBlock
  def reify(f: (Exp, Exp, Exp) => Exp): Block = dummyBlock
  def reify(f: (Exp, Exp, Exp, Exp) => Exp): Block = dummyBlock
  def reify(arity: Int, f: List[Exp] => Exp, here: Boolean = false): Block =
    dummyBlock

  def getEffKeys(b: Block) = (Set[Exp](), Set[Exp]())
  def mergeEffKeys(b: Block, c: Block) = (Set[Exp](), Set[Exp]())
}

class GraphBuilderOpt extends GraphBuilder {}

case class Graph(
    val nodes: Seq[Node],
    val block: Block,
    val globalDefsCache: immutable.Map[Sym, Node]
) {}

abstract class Phase extends (Graph => Graph) {}

class DeadCodeElimCG extends Phase {
  var live: collection.Set[Sym] = _
  var reach: collection.Set[Sym] = _
  var statics: collection.Set[Node] = _

  def apply(g: Graph): Graph = g
}

class Bound extends Phase {
  val hm = new mutable.HashMap[Sym, Set[Sym]]
  def apply(g: Graph): Graph = g
}
