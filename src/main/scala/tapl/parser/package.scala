package tapl
import tapl.core._

import scala.util.parsing.combinator.JavaTokenParsers

package object parser extends JavaTokenParsers {
  def term: Parser[Context => Term] = tmvar | tmabs | tmapp

  def tmtype: Parser[Context => Ty] =
    "Bool" ^^ { _ => ctx: Context => TyBool } | ("(" ~ tmtype ~ "->" ~ tmtype ~ ")") ^^ {
      case _ ~ tyT1 ~ _ ~ tyT2 ~ _ =>
        ctx: Context => TyArrow(tyT1(ctx), tyT2(ctx))
    }

  def tmapp: Parser[Context => TmApp] =
    "(" ~ term ~ term ~ ")" ^^ {
      case _ ~ t1 ~ t2 ~ _ => ctx: Context => TmApp(t1(ctx), t2(ctx))
    }
  def tmvar: Parser[Context => TmVar] =
    ident ^^ { x => ctx: Context => TmVar(ctx.name2index(x), ctx.len()) }
  def tmabs: Parser[Context => TmAbs] =
    "(" ~ "lambda" ~ ident ~ ":" ~ tmtype ~ "." ~ term ~ ")" ^^ {
      case _ ~ _ ~ ident ~ _ ~ ty ~ _ ~ t1 ~ _ =>
        ctx: Context =>
          val (c2, x2) = ctx.pickFreshName(ident)
          if (ident != x2)
            println(
              s"Warning: Name conflict is not well supported: ${ident} ${x2}"
            )
          TmAbs(x2, ty(c2), t1(c2))
    }

  def startParse(s: String): Option[Term] =
    parseAll(term, s) match {
      case Success(r, _) => Some(r(Context(Nil)))
      case _             => None
    }
}
