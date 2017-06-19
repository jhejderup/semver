package io.github.jhejderup.semver.lexer

import io.github.jhejderup.semver.compiler.{Location, SemverLexerError}

import scala.util.parsing.combinator.RegexParsers


object SemverLexer extends RegexParsers {


  override def skipWhitespace = false

  def apply(code: String): Either[SemverLexerError, List[SemverToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(SemverLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }


  def tokens: Parser[List[SemverToken]] = {
    phrase(rep(lessthanequals | greaterthanequals | greaterthan | lessthan | equals | lowercasex
      | uppercasex | star | tilde | caret | plus | minus | or | dot | number | prereleaseidentifier
      | whitepace)) ^^ { rawTokens => postprocess(rawTokens)
    }
  }

  private def postprocess(tokens: List[SemverToken]): List[SemverToken] =
    if (tokens.size > 0) tokens else EMPTY() :: tokens


  def number: Parser[NUMBER] = {
    """(0|[1-9]\d*)""".r ^^ { str => NUMBER(str.toInt) }
  }

  def prereleaseidentifier: Parser[PRERELEASEIDENTIFIER] = {
    """[-0-9A-Za-z]+""".r ^^ { str => PRERELEASEIDENTIFIER(str) }
  }

  def whitepace = positioned {
    " " ^^^ WHITESPACE()
  }

  def or = positioned {
    "||" ^^^ ORSIGN()
  }

  def lessthan = positioned {
    "<" ^^^ LESSTHAN()
  }

  def greaterthan = positioned {
    ">" ^^^ GREATERTHAN()
  }

  def greaterthanequals = positioned {
    ">=" ^^^ GREATERTHANEQUALS()
  }

  def lessthanequals = positioned {
    "<=" ^^^ LESSTHANEQUALS()
  }

  def equals = positioned {
    "=" ^^^ EQUALS()
  }

  def dot = positioned {
    "." ^^^ DOT()
  }

  def lowercasex = positioned {
    "x" ^^^ LOWERCASEX()
  }

  def uppercasex = positioned {
    "X" ^^^ UPPERCASEX()
  }

  def star = positioned {
    "*" ^^^ STAR()
  }

  def tilde = positioned {
    "~" ^^^ TILDE()
  }

  def caret = positioned {
    "^" ^^^ CARET()
  }

  def minus = positioned {
    "-" ^^^ MINUS()
  } //same as hyphen
  def plus = positioned {
    "+" ^^^ PLUS()
  }


}
