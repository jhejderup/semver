package io.github.jhejderup.semver.lexer

import scala.util.parsing.input.Positional

sealed trait SemverToken extends Positional

case class NUMBER(str: Int) extends SemverToken

case class PRERELEASEIDENTIFIER(str: String) extends SemverToken

case class WHITESPACE() extends SemverToken

case class ORSIGN() extends SemverToken

case class EMPTY() extends SemverToken

case class DOT() extends SemverToken

case class LOWERCASEX() extends SemverToken

case class UPPERCASEX() extends SemverToken

case class STAR() extends SemverToken

case class TILDE() extends SemverToken

case class CARET() extends SemverToken

case class MINUS() extends SemverToken

case class PLUS() extends SemverToken

case class LESSTHAN() extends SemverToken

case class GREATERTHAN() extends SemverToken

case class GREATERTHANEQUALS() extends SemverToken

case class LESSTHANEQUALS() extends SemverToken

case class EQUALS() extends SemverToken
