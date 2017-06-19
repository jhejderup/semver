package io.github.jhejderup.semver.parser

import io.github.jhejderup.semver.compiler.{Location, SemverParserError}
import io.github.jhejderup.semver.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object SemverParser extends Parsers {
  override type Elem = SemverToken


  class SemverTokenReader(tokens: Seq[SemverToken]) extends Reader[SemverToken] {
    override def first: SemverToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[SemverToken] = new SemverTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[SemverToken]): Either[SemverParserError, SemverAST] = {
    val reader = new SemverTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(SemverParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[SemverAST] = {
    phrase(semverRange)
  }


  def semverRange: Parser[SemverAST] = {
    val logicalOr = rep(WHITESPACE()) ~ ORSIGN() ~ rep(WHITESPACE())
    range ~ rep(logicalOr ~ range) ^^ {
      case range ~ List() => SemverRange(range)
      case range ~ rangeSet =>  SemverRange(Or(range :: rangeSet.map(_._2)))
    }
  }

  def range: Parser[SemverAST] = hyphen | simple ~ rep(WHITESPACE() ~ simple) ^^ {
    case r ~ List() => r
    case r ~ rSet => And(r :: rSet.map(_._2))
  } | empty


  def empty: Parser[SemverAST] = EMPTY() ^^^ SemverID(STAR())
  def simple: Parser[SemverAST] =  primitive | partial | tilde | caret


  def hyphen: Parser[SemverAST] = partial ~ WHITESPACE() ~ MINUS() ~ WHITESPACE() ~ partial ^^ {
    case lhs ~ WHITESPACE() ~ MINUS() ~ WHITESPACE() ~ rhs => HyphenRange(lhs, rhs)
  }

  def primitive: Parser[SemverAST] = (GREATERTHANEQUALS() | LESSTHANEQUALS() |
    GREATERTHAN() | LESSTHAN() | EQUALS()) ~ partial ^^ {
    case op ~ partial => CompareRange(op, partial)
  }

  def tilde: Parser[SemverAST] = TILDE() ~ partial ^^ {
    case _ ~ partial => TildeRange(partial)
  }

  def caret: Parser[SemverAST] = CARET() ~ partial ^^ {
    case _ ~ partial => CaretRange(partial)
  }

  def partial: Parser[SemverID] = xr ~ opt(DOT() ~ xr ~ opt(DOT() ~ xr ~ opt(qualifier))) ^^ {
    case major ~ None => SemverID(major)
    case major ~ Some(DOT() ~ minor ~ None) => SemverID(major, Some(minor))
    case major ~ Some(DOT() ~ minor ~ Some(DOT() ~ patch ~ Some(PreReleaseTags(None,None))))
    => SemverID(major, Some(minor), Some(patch))
    case major ~ Some(DOT() ~ minor ~ Some(DOT() ~ patch ~ Some(PreReleaseTags(pre,None))))
    => SemverID(major, Some(minor), Some(patch), pre)
    case major ~ Some(DOT() ~ minor ~ Some(DOT() ~ patch ~ Some(PreReleaseTags(None, build))))
    => SemverID(major, Some(minor), Some(patch), None , build)
    case major ~ Some(DOT() ~ minor ~ Some(DOT() ~ patch ~ Some(PreReleaseTags(pre, build))))
    => SemverID(major, Some(minor), Some(patch), pre , build)

  }

  def nr: Parser[SemverToken] = number

  def xr: Parser[SemverToken] = LOWERCASEX() | UPPERCASEX() | STAR() | nr

  def qualifier: Parser[PreReleaseTags] = opt(MINUS() ~ pre) ~ opt(PLUS() ~ build)  ^^ {
    case None ~ None => PreReleaseTags()
    case Some(MINUS() ~ pre) ~ None => PreReleaseTags(Some(pre))
    case None ~ Some(PLUS() ~ build) => PreReleaseTags(None, Some(build))
    case Some(MINUS() ~ pre) ~ Some(PLUS() ~ build)  => PreReleaseTags(Some(pre), Some(build))

  }

  def pre: Parser[List[SemverToken]] = parts

  def build: Parser[List[SemverToken]] = parts

  def parts: Parser[List[SemverToken]] = part ~ rep(DOT() ~ part) ^^ {
    case part ~ List() => part :: List[SemverToken]()
    case part ~ parts => part :: parts.map(_._2)
  }

  def part: Parser[SemverToken] = nr | identifier


  private def number: Parser[NUMBER] = positioned {
    accept("number", { case digit@NUMBER(str) => digit })
  }


  private def identifier: Parser[PRERELEASEIDENTIFIER] = positioned {
    accept("identifier", { case id@PRERELEASEIDENTIFIER(str) => id })
  }

}