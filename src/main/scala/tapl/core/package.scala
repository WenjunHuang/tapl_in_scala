package tapl

package object core {


  def typeString(ty: Ty): String =
    ty match {
      case TyBool => "Bool"
      case TyArrow(left, right) =>
        "(" + typeString(left) + " -> " + typeString(right) + ")"
    }

  def termString(ctx: Context, term: Term): String =
    term match {
      case TmAbs(x, ty, t) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        s"(lambda $x1: ${typeString(ty)}. ${termString(ctx1, t)})"
      case TmVar(x, n) =>
        if (ctx.len() == n)
          s"${ctx.index2name(x)}"
        else {
          throw new IndexOutOfBoundsException("[bad index]")
        }
      case TmApp(t1, t2) =>
        s"(${termString(ctx, t1)} ${termString(ctx, t2)})"
    }
}
