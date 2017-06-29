package io.github.jhejderup.semver.lexer

import io.github.jhejderup.semver.compiler.{Location, SemverLexerError}

import scala.util.parsing.combinator.RegexParsers


object SemverLexer extends RegexParsers {
  override def skipWhitespace = false
  override protected val whiteSpace = """( |\\t|\t)""".r

  def apply(code: String): Either[SemverLexerError, List[SemverToken]] = {

      parse(tokens, code) match {
        case NoSuccess(msg, next) => Left(SemverLexerError(Location(next.pos.line, next.pos.column), msg))
        case Success(result, next) => Right(result)
      }
  }


  def tokens: Parser[List[SemverToken]] = {
    phrase(rep(lessthanequals | greaterthanequals | greaterthan | lessthan | equals | lowercasex
      | uppercasex | star | tilde | caret | plus | union | number
      | dot | minus | prereleaseidentifier |  whitepace)) ^^ { rawTokens => postprocess(rawTokens)
    }
  }


  private def postprocess(tokens: List[SemverToken]): List[SemverToken] =
    if (tokens.size > 0) tokens else EMPTY :: tokens


  def number: Parser[NUMBER] = {
    """(0|[1-9]\d*)""".r ^^ { str => NUMBER(str.toInt) }
  }

  def prereleaseidentifier: Parser[PREID] = {
    """[-0-9A-Za-z]+""".r ^^ { str => PREID(str) }
  }

  def whitepace = positioned {
    " " ^^^ WHITESPACE
  }

  def union = positioned {
    "||" ^^^ UNION
  }

  def lessthan = positioned {
    "<" ^^^ LT
  }

  def greaterthan = positioned {
    ">" ^^^ GT
  }

  def greaterthanequals = positioned {
    ">=" ^^^ GTE
  }

  def lessthanequals = positioned {
    "<=" ^^^ LTE
  }

  def equals = positioned {
    "=" ^^^ EQU
  }

  def dot = positioned {
    "." ^^^ DOT
  }

  def lowercasex = positioned {
    "x" ^^^ LETTERX
  }

  def uppercasex = positioned {
    "X" ^^^ LETTERX
  }

  def star = positioned {
    "*" ^^^ STAR
  }

  def tilde = positioned {
    """(?:~>?)""".r ^^^ TILDE
  }

  def caret = positioned {
    "^" ^^^ CARET
  }

  def minus = positioned {
    "-" ^^^ MINUS
  } //same as hyphen
  def plus = positioned {
    "+" ^^^ PLUS
  }


}
