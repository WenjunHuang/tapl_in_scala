package tapl.core

sealed trait Binding
case object NameBind extends Binding
case class VarBind(ty: Ty) extends Binding

case class Context(contexts: List[(String, Binding)]) {
  def addBinding(x: String, binding: Binding): Context = {
    contexts.find(_._1 == x) match {
      case None    => Context(contexts :+ (x -> binding))
      case Some(_) => this
    }
  }

  def getBinding(index: Int): Option[Binding] =
    if (contexts.isDefinedAt(index)) Some(contexts(len() - index - 1)._2)
    else None

  def getTypeFromContext(index: Int): Either[String, Ty] =
    getBinding(index) match {
      case Some(VarBind(ty)) => Right(ty)
      case _                 => Left(s"no type for index: ${index}")
    }

  def pickFreshName(nameHint: String): (Context, String) = {
    if (contexts.exists(v => v._1 == nameHint && v._2 == NameBind))
      pickFreshName(nameHint + "'")
    else {
      (Context(contexts :+ (nameHint -> NameBind)), nameHint)
    }
  }

  def len(): Int = contexts.size

  def index2name(index: Int): String = {
    if (index < len)
      contexts(len() - 1 - index)._1
    else {
      throw new IndexOutOfBoundsException(
        s"variable lookup fail. index:$index, context length:${len()}"
      )
    }
  }

  def name2index(s: String): Int = {
    contexts.zipWithIndex.find(v => v._1._1 == s && v._1._2 == NameBind) match {
      case Some((_, i)) => len() - 1 - i
      case None =>
        throw new IllegalArgumentException(s"variable lookup fails: $s")
    }
  }
}
