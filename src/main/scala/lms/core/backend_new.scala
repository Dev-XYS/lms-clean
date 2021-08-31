package lms.core

import scala.collection.{immutable, mutable}

object Backend {
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {}
  case class Const(x: Any) extends Exp {}

  case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary) {}

  abstract class Def

  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary)
      extends Def {
    def bound: List[Sym] = List()
    def used: Set[Sym] = Set()
    def deps: Set[Sym] = Set()
    def isPure = true
  }

  implicit val orderingExp: Ordering[Exp] = Ordering.by(e =>
    e match {
      case Const(s) => -s.##.abs
      case Sym(n)   => n
    }
  )
  implicit val orderingSym: Ordering[Sym] = Ordering.by(_.n)

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

    def filter(f: Sym => Boolean) = {
      val nf = (e: Exp) =>
        e match {
          case s: Sym => f(s)
          case _      => true
        }
      EffectSummary(
        sdeps.filter(nf),
        hdeps.filter(nf),
        rkeys.filter(nf),
        wkeys.filter(nf)
      )
    }
  }

  def blocks(x: Node): List[Block] = List()
  def directSyms(x: Node): List[Sym] = List()
  def syms(x: Node): List[Sym] = List()
  def hardSyms(x: Node): Set[Sym] = Set()
  def boundSyms(x: Node): Seq[Sym] = Seq()

  final val emptySummary = EffectSummary(Set(), Set(), Set(), Set())
  final def softSummary(s: Sym) = EffectSummary(Set(s), Set(), Set(), Set())
  final def hardSummary(s: Sym) = EffectSummary(Set(), Set(s), Set(), Set())
  final def writeSummary(s: Exp) = EffectSummary(Set(), Set(), Set(), Set(s))
}

import Backend._

class GraphBuilder {
  val dummySym = Sym(-1)
  val dummyEffect = EffectSummary(Set(), Set(), Set(), Set())
  val dummyBlock = Block(List(), dummySym, dummySym, dummyEffect)

  val globalDefs = new mutable.ArrayBuffer[Node]
  val globalDefsCache = new mutable.HashMap[Sym, Node]
  val globalDefsReverseCache = new mutable.HashMap[(String, Seq[Def]), Node]

  var nSyms = 0
  def fresh = try nSyms
  finally nSyms += 1

  object Def {
    def unapply(xs: Def): Option[(String, List[Def])] = None
  }

  def findDefinition(s: Exp): Option[Node] = None

  def rewrite(s: String, as: List[Def]): Option[Exp] = None

  def reflect(x: Sym, s: String, as: Def*)(
      summary: EffectSummary = emptySummary
  ): Sym = {
    val n = Node(x, s, as.toList, summary)
    globalDefs += n
    globalDefsCache(x) = n
    globalDefsReverseCache((s, n.rhs)) = n
    x
  }
  def reflect(s: String, as: Def*): Exp = {
    reflect(Sym(fresh), s, as: _*)()
  }
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
) {
  override def toString = {
    val source = new java.io.ByteArrayOutputStream()
    val stream = new java.io.PrintStream(source)
    stream.println("=================")
    for (node <- nodes)
      stream.println(node)
    stream.println(block)
    stream.println("=================")
    source.toString
  }
}

abstract class Phase extends (Graph => Graph) {}

// Compute liveness information and discard
// nodes not used in computing the result
class DeadCodeElim extends Phase {
  def apply(g: Graph): Graph = {

    val live = new mutable.HashSet[Sym]

    if (g.block.res.isInstanceOf[Sym])
      live += g.block.res.asInstanceOf[Sym]

    for (d <- g.nodes.reverseIterator)
      if (live(d.n)) live ++= syms(d)

    g.copy(nodes = g.nodes.filter(d => live(d.n)))
  }
}

/*
 * Resolve may dependencies
 */
class DeadCodeElimCG extends Phase {

  final val fixDeps = true // remove dependencies on removed nodes
  // it is interesting to discuss the difference between `live` and `reach`.
  // `reach` is the set of nodes (or Syms of nodes) that are reachable via hard-dependencies.
  // the reachable set can include values that are needed (data dependencies) and side-effects
  // (printing, mutation, et. al.)
  // `live` is the set of nodes (or Syms of nodes) whose values are needed (only data dependencies).
  // For instance, a conditional can have side-effects and return values. If the side-effects
  // are relevant, then the conditional is reachable. If the values are relevant, the conditional
  // is live. The property of `live` and `reach` can be independent.
  var live: collection.Set[Sym] = _
  var reach: collection.Set[Sym] = _

  def valueSyms(n: Node): List[Sym] =
    directSyms(n) ++ blocks(n).flatMap {
      case Block(ins, res: Sym, ein, _) => res :: ein :: ins
      case Block(ins, _, ein, _)        => ein :: ins
    }

  // staticData -- not really a DCE task, but hey
  var statics: collection.Set[Node] = _

  def apply(g: Graph): Graph = utils.time("DeadCodeElimCG") {

    live = new mutable.HashSet[Sym]
    reach = new mutable.HashSet[Sym]
    statics = new mutable.HashSet[Node]
    var newNodes: List[Node] = Nil
    val used = new mutable.HashSet[Exp]
    var size = 0

    // First pass liveness and reachability
    // Only a single pass that reduce input size and first step of the next loop
    utils.time("A_First_Path") {
      reach ++= g.block.used
      if (g.block.res.isInstanceOf[Sym]) {
        live += g.block.res.asInstanceOf[Sym]
        used += g.block.res.asInstanceOf[Sym]
      }
      used ++= g.block.bound
      for (d <- g.nodes.reverseIterator) {
        if (reach(d.n)) {
          val nn = d match {
            case n @ Node(s, "?", c :: (a: Block) :: (b: Block) :: t, eff)
                if !live(s) =>
              n.copy(rhs =
                c :: a.copy(res = Const(())) :: b.copy(res = Const(())) :: t
              ) // remove result deps if dead
            case _ => d
          }
          live ++= valueSyms(nn)
          reach ++= hardSyms(nn)

          newNodes = nn :: newNodes
        }
      }
    }

    // Second pass remove unused variables
    var idx: Int = 1
    while (size != used.size) {
      utils.time(s"Extra_Path_$idx") {
        size = used.size
        for (d <- newNodes.reverseIterator) {
          if (used(d.n)) {
            used ++= valueSyms(d)
          } else if (d.eff.hasSimpleEffect || d.eff.wkeys.exists(used)) {
            used += d.n
            used ++= valueSyms(d)
          }
        }
      }
      idx += 1
    }

    utils.time(s"Recreate_the_graph") {
      var newGlobalDefsCache = Map[Sym, Node]()
      newNodes = for (d <- newNodes if used(d.n)) yield {
        newGlobalDefsCache += d.n -> d
        if (d.op == "staticData") statics += d
        if (fixDeps)
          d.copy(
            rhs = d.rhs.map {
              case b: Block => b.copy(eff = b.eff.filter(used))
              case o        => o
            },
            eff = d.eff.filter(used)
          )
        else
          d
      }
      val newBlock =
        if (fixDeps)
          g.block.copy(eff = g.block.eff.filter(used))
        else
          g.block

      Graph(newNodes, newBlock, newGlobalDefsCache)
    }
  }
}

// Compute bound structure information
// (i.e., which bound variables a node depends on)
class Bound extends Phase {

  val hm = new mutable.HashMap[Sym, Set[Sym]]

  def apply(g: Graph): Graph = {
    val bound = g.nodes.flatMap(boundSyms).toSet ++ g.block.bound

    // For recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    // for (Node(b,"λ",_,_) <- g.nodes)
    //   hm(b) = Set()
    //
    // This explicit initialization was previously needed
    // since we used hm.getOrElse(a,Set(a)) as default below
    // (there is a choice whether undefined symbols should be
    // treated as bound or not -- this case is typically only
    // encountered for recursive lambda refs).

    for (b <- bound)
      hm(b) = Set(b)

    // Convergence loop: we want to make sure that recursive
    // refs via λforward nodes acquire the same bound information
    // as the lambda itself (which comes later in the list) and
    // hence get scheduled into the same block (see recursion_3 test).

    var more = true
    while (more) {
      more = false

      for (d <- g.nodes) {
        val b = boundSyms(d).toSet - d.n
        val newSyms = syms(d).flatMap(a => hm.getOrElse(a, Set())).toSet -- b
        more ||= (d.op == "λforward" && hm.get(d.n) != Some(newSyms))
        hm(d.n) = newSyms
      }
    }

    //hm.foreach(println)

    g
  }

}

// Compute frequency information (i.e., how
// many times a node's result is going to
// be used, in expectation)
class Flow extends Phase {

  val freq = new mutable.HashMap[Sym, Double]

  // XXX: not clear how to count effect deps
  // (e.g. 'if' shouldn't count effect as 0.5, b/c together with
  // a normal dep this is 1.0, and would thus hoist stuff)

  // XXX perhaps we need to count freqs differently (?)

  def symsFreq(x: Node): List[(Def, Double)] = x match {
    case Node(f, "λ", Block(in, y, ein, eff) :: _, _) =>
      List((y, 100.0)) ++ eff.deps.map(e => (e, 0.001))
    case Node(
          _,
          "?",
          c :: Block(ac, ae, ai, af) :: Block(bc, be, bi, bf) :: _,
          _
        ) =>
      List((c, 1.0), (ae, 0.5), (be, 0.5)) ++ af.deps.map(e =>
        (e, 0.001)
      ) ++ bf.deps.map(e => (e, 0.001))
    case _ => syms(x) map (s => (s, 1.0))
  }

  def apply(g: Graph): Graph = {

    if (g.block.res.isInstanceOf[Sym])
      freq(g.block.res.asInstanceOf[Sym]) = 1.0

    for (e <- g.block.eff.deps)
      if (e.isInstanceOf[Sym])
        freq(e.asInstanceOf[Sym]) = 1.0

    for (d <- g.nodes.reverseIterator) {
      if (freq contains d.n) {
        val s = freq(d.n)
        for ((e: Sym, f) <- symsFreq(d))
          if (f > 0.5) freq(e) = (freq.getOrElse(e, 0.0) + (f * s))
      }
    }

    //freq.toList.sortBy(_._1.n).foreach(println)

    g
  }
}
