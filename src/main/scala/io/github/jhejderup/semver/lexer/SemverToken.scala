package io.github.jhejderup.semver.lexer

import scala.util.parsing.input.Positional

sealed trait SemverToken extends Positional

case class NUMBER(str: Int) extends SemverToken {
  override def toString: String = s"$str"
}

case class PREID(str: String) extends SemverToken {
  override def toString: String = str
}

case object WHITESPACE extends SemverToken

case object UNION extends SemverToken {
  override def toString: String = "||"
}

case object EMPTY extends SemverToken {
  override def toString: String = ""
}

case object DOT extends SemverToken {
  override def toString: String = "."
}

case object LETTERX extends SemverToken {
  override def toString: String = "x"
}

case object STAR extends SemverToken {
  override def toString: String = "*"
}

case object TILDE extends SemverToken {
  override def toString: String = "~"
}

case object CARET extends SemverToken {
  override def toString: String = "^"
}

case object MINUS extends SemverToken {
  override def toString: String = "-"
}

case object PLUS extends SemverToken {
  override def toString: String = "+"
}

case object LT extends SemverToken {
  override def toString: String = "<"
}

case object GT extends SemverToken {
  override def toString: String = ">"
}

case object GTE extends SemverToken {
  override def toString: String = ">="
}

case object LTE extends SemverToken {
  override def toString: String = "<="
}

case object EQU extends SemverToken {
  override def toString: String = "="
}
