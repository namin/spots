// Hindler-Milner Type Inference from Scala by Example, ch. 16

abstract class Term {}
case class Var(x: String) extends Term {
  override def toString = x
}
case class Lam(x: String, e: Term) extends Term {
  override def toString = "(\\" + x + "." + e + ")"
}
case class App(f: Term, e: Term) extends Term {
  override def toString = "(" + f + " " + e + ")"
}
case class Let(x: String, e: Term, f: Term) extends Term {
  override def toString = "let " + x + " = " + e + " in " + f
}
case class Letrec(x: String, e: Term, f: Term) extends Term {
  override def toString = "letrec " + x + " = " + e + " in " + f
}

sealed abstract class Type {}
case class Tyvar(a: String) extends Type {
  override def toString = a
}
case class Arrow(t1: Type, t2: Type) extends Type {
  override def toString = "(" + t1 + "->" + t2 + ")"
}
case class Tycon(k: String, ts: List[Type]) extends Type {
  override def toString =
    k + (if (ts.isEmpty) "" else ts.mkString("[", ",", "]"))
}

object typeInfer {
  private var n: Int = 0
  def newTyvar(): Type = { n += 1; Tyvar("a" + n) }

  abstract class Subst extends Function1[Type,Type] {
    def lookup(x: Tyvar): Type
    def apply(t: Type): Type = t match {
      case tv @ Tyvar(a) => val u = lookup(tv); if (t == u) t else apply(u)
      case Arrow(t1, t2) => Arrow(apply(t1), apply(t2))
      case Tycon(k, ts) => Tycon(k, ts map apply)
    }
    def apply(env: Env): Env = env.map({ case (x, TypeScheme(tyvars, tpe)) =>
      // assumes tyvars don't occur in this substitution
      (x, TypeScheme(tyvars, apply(tpe)))
    })
    def extend(x: Tyvar, t: Type) = new Subst {
      def lookup(y: Tyvar): Type = if (x == y) t else Subst.this.lookup(y)
    }
  }
  val emptySubst = new Subst { def lookup(t: Tyvar): Type = t }

  case class TypeScheme(tyvars: List[Tyvar], tpe: Type) {
    def newInstance: Type = {
      (emptySubst /: tyvars) ((s, tv) => s.extend(tv, newTyvar())) (tpe)
    }
  }

  type Env = List[(String, TypeScheme)]

  def lookup(env: Env, x: String): TypeScheme = env match {
    case List() => null
    case (y, t) :: env1 => if (x == y) t else lookup(env1, x)
  }

  def gen(env: Env, t: Type): TypeScheme =
    TypeScheme(tyvars(t) diff tyvars(env), t)

  def tyvars(t: Type): List[Tyvar] = t match {
    case tv @ Tyvar(a) =>
      List(tv)
    case Arrow(t1, t2) =>
      tyvars(t1) union tyvars(t2)
    case Tycon(k, ts) =>
      (List[Tyvar]() /: ts) ((tvs, t) => tvs union tyvars(t))
  }

  def tyvars(ts: TypeScheme): List[Tyvar] =
    tyvars(ts.tpe) diff ts.tyvars

  def tyvars(env: Env): List[Tyvar] =
    (List[Tyvar]() /: env) ((tvs, nt) => tvs union tyvars(nt._2))

  def mgu(t: Type, u: Type, s: Subst): Subst = (s(t), s(u)) match {
    case (Tyvar(a), Tyvar(b)) if (a == b) =>
      s
    case (st @ Tyvar(a), su) if !(tyvars(su) contains st) =>
      s.extend(st, su)
    case (_, Tyvar(a)) =>
      mgu(u, t, s)
    case (Arrow(t1, t2), Arrow(u1, u2)) =>
      mgu(t1, u1, mgu(t2, u2, s))
    case (Tycon(k1, ts), Tycon(k2, us)) if (k1 == k2) =>
      (s /: (ts zip us)) ((s, tu) => mgu(tu._1, tu._2, s))
    case _ =>
      throw new TypeError("cannot unify " + s(t) + " with " + s(u))
  }

  case class TypeError(s: String) extends Exception(s) {}

  def tp(env: Env, e: Term, t: Type, s: Subst): Subst = {
    current = e
    e match {
      case Var(x) =>
	val u = lookup(env, x)
        if (u == null) throw new TypeError("undefined: " + x)
        else mgu(u.newInstance, t, s)
      case Lam(x, e1) =>
	val a, b = newTyvar()
        val s1 = mgu(t, Arrow(a, b), s)
        val env1 = (x, TypeScheme(List(), a)) :: env
        tp(env1, e1, b, s1)
      case App(e1, e2) =>
	val a = newTyvar()
        val s1 = tp(env, e1, Arrow(a, t), s)
        tp(env, e2, a, s1)
      case Let(x, e1, e2) =>
	val a = newTyvar()
        val s1 = tp(env, e1, a, s)
        tp((x, gen(s1(env), s1(a))) :: env, e2, t, s1)
      case Letrec(x, e1, e2) =>
	val a, b = newTyvar()
        val s1 = tp((x, TypeScheme(List(), a)) :: env, e1, b, s)
        val s2 = mgu(a, b, s1)
        tp((x, gen(s2(env), s2(a))) :: env, e2, t, s2)
    }
  }
  var current: Term = null

  def typeOf(env: Env, e: Term): Type = {
    val a = newTyvar()
    tp(env, e, a, emptySubst)(a)
  }
}// end typeInfer

object predefined {
  val booleanType = Tycon("Boolean", List())
  val intType = Tycon("Int", List())
  def listType(t: Type) = Tycon("List", List(t))
  private def gen(t: Type): typeInfer.TypeScheme = typeInfer.gen(List(), t)
  private val a = typeInfer.newTyvar()
  val env = List(
    ("true", gen(booleanType)),
    ("false", gen(booleanType)),
    ("if", gen(Arrow(booleanType, Arrow(a, Arrow(a, a))))),
    ("zero", gen(intType)),
    ("succ", gen(Arrow(intType, intType))),
    ("nil", gen(listType(a))),
    ("cons", gen(Arrow(a, Arrow(listType(a), listType(a))))),
    ("isEmpty", gen(Arrow(listType(a), booleanType))),
    ("head", gen(Arrow(listType(a), a))),
    ("tail", gen(Arrow(listType(a), listType(a)))),
    ("fix", gen(Arrow(Arrow(a, a), a)))
  )
}

object testInfer {
  def showType(e: Term): String =
    try {
      typeInfer.typeOf(predefined.env, e).toString
    } catch {
      case typeInfer.TypeError(msg) =>
	"\n cannot type: " + typeInfer.current +
      "\n reason: " + msg
    }
}
