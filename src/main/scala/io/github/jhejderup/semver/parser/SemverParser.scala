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
    val logicalOr = rep(WHITESPACE) ~ UNION ~ rep(WHITESPACE)
    range ~ rep(logicalOr ~ range) ^^ {
      case range ~ List() => range
      case range ~ rangeSet =>  Union(range :: rangeSet.map(_._2))
    }
  }

  def range: Parser[SemverAST] = hyphen | simple ~ rep(WHITESPACE ~ simple) ^^ {
    case r ~ List() => r
    case r ~ rSet => Intersection(r :: rSet.map(_._2))
  } | empty


  def empty: Parser[SemverAST] = (EMPTY | UNION) ^^^ Semver(STAR, STAR, STAR)
  def simple: Parser[SemverAST] =  primitive | partial | tilde | caret


  def hyphen: Parser[SemverAST] = partial ~ WHITESPACE ~ MINUS ~ WHITESPACE ~ partial ^^ {
    case lhs ~ WHITESPACE ~ MINUS ~ WHITESPACE ~ rhs => HyphenRange(lhs, rhs)
  }

  def primitive: Parser[SemverAST] = (GTE | LTE | GT | LT | EQU) ~ rep(WHITESPACE) ~ partial ^^ {
    case op ~ _ ~ partial => CompareRange(op, partial)
  }

  def tilde: Parser[SemverAST] = TILDE ~ rep(WHITESPACE)  ~ partial ^^ {
    case _ ~ partial => TildeRange(partial)
  }

  def caret: Parser[SemverAST] = CARET ~ rep(WHITESPACE) ~ partial ^^ {
    case _ ~ partial => CaretRange(partial)
  }


  //Any of X, x, or * may be used to "stand in" for one of the numeric values in the [major, minor, patch] tuple.
  def partial: Parser[Semver] = (rep(WHITESPACE) ~ (EQU.? | PREID("v").?) ~ rep(WHITESPACE)) ~> xr ~ opt(DOT ~ xr ~ opt(DOT ~ xr ~ opt(qualifier))) ^^ {
    case major ~ None => Semver(major, LETTERX, LETTERX)
    case major ~ Some(DOT ~ minor ~ None) => Semver(major, minor, LETTERX)
    case major ~ Some(DOT ~ minor ~ Some(DOT ~ patch ~ Some(PreReleaseTags(None,None))))
    => Semver(major, minor, patch)
    case major ~ Some(DOT ~ minor ~ Some(DOT ~ patch ~ Some(PreReleaseTags(pre,None))))
    => Semver(major, minor, patch, pre)
    case major ~ Some(DOT ~ minor ~ Some(DOT ~ patch ~ Some(PreReleaseTags(None, build))))
    => Semver(major, minor, patch, None , build)
    case major ~ Some(DOT ~ minor ~ Some(DOT ~ patch ~ Some(PreReleaseTags(pre, build))))
    => Semver(major, minor, patch, pre , build)

  }


  def nr: Parser[SemverToken] = number

  def xr: Parser[SemverToken] = cleannumber |LETTERX | STAR | nr

  def qualifier: Parser[PreReleaseTags] = opt(MINUS ~ pre) ~ opt(PLUS ~ build)  ^^ {
    case None ~ None => PreReleaseTags()
    case Some(MINUS ~ pre) ~ None => PreReleaseTags(Some(pre))
    case None ~ Some(PLUS ~ build) => PreReleaseTags(None, Some(build))
    case Some(MINUS ~ pre) ~ Some(PLUS ~ build)  => PreReleaseTags(Some(pre), Some(build))

  }

  def pre: Parser[List[SemverToken]] = parts

  def build: Parser[List[SemverToken]] = parts


  def parts: Parser[List[SemverToken]] = (dashpart | part) ~ rep(DOT ~ (dashpart | part)) ^^ {
    case part ~ List() => part :: List[SemverToken]()
    case part ~ parts => part :: parts.map(_._2)
  }

  def dashpart: Parser[SemverToken] = (nr ~ MINUS | MINUS) ~ rep(part| MINUS) ^^ {
    case num ~ MINUS ~ List() => PREID(num.toString + MINUS)
    case num ~ List() => PREID(num.toString)
    case num ~ MINUS ~ parts => PREID(num.toString + MINUS + parts.mkString(""))
    case num ~ parts => PREID(num.toString + parts.mkString(""))
  }


  def part: Parser[SemverToken] =   nr | identifier


  private def number: Parser[NUMBER] = positioned {
    accept("number", { case digit@NUMBER(str) => digit })
  }


  private def identifier: Parser[PREID] = positioned {
    accept("identifier", { case id@PREID(str) => id })
  }


  private def cleannumber: Parser[NUMBER] = positioned {
    accept("identifier", { case PREID(str) => NUMBER(str.replaceAll("^[=v]+","").toInt)})
  }

}