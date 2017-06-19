package io.github.jhejderup.semver.parser

import io.github.jhejderup.semver.lexer._

sealed trait SemverAST {

  def transform(): SemverAST

}

case class SemverID(major: SemverToken, minor: Option[SemverToken] = None, patch: Option[SemverToken] = None,
                    preRelease: Option[List[SemverToken]] = None,
                    metadata: Option[List[SemverToken]] = None) extends SemverAST{
  override def transform(): SemverAST = {
    this match {
      case SemverID(LOWERCASEX() | UPPERCASEX() | STAR(), _, _, _, _) => CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(0), Some(NUMBER(0)), Some(NUMBER(0))))
      case SemverID(NUMBER(major), None | Some(LOWERCASEX() | UPPERCASEX() | STAR()), _, pre, build) =>
        And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major),Some(NUMBER(0)), Some(NUMBER(0)), pre, build)),CompareRange(LESSTHAN(),SemverID(NUMBER(major+1),Some(NUMBER(0)), Some(NUMBER(0))))))
      case SemverID(NUMBER(major), Some(NUMBER(minor)), None | Some(LOWERCASEX() | UPPERCASEX() | STAR()), pre, build) =>
        And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(0)), pre, build)),CompareRange(LESSTHAN(),SemverID(NUMBER(major),Some(NUMBER(minor+1)), Some(NUMBER(0))))))
      case _ => this
    }

  }

}

case class PreReleaseTags(pre:Option[List[SemverToken]] = None, build:Option[List[SemverToken]] = None)

case class CaretRange(partialRange: SemverID) extends SemverAST {
  override def transform(): SemverAST = partialRange match {
    case id@SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(patch)),pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), id), CompareRange(LESSTHAN(), SemverID(NUMBER(major+1), Some(NUMBER(0)), Some(NUMBER(0))))))
    case SemverID(NUMBER(major), Some(NUMBER(minor)), None | Some(UPPERCASEX() | LOWERCASEX() | STAR()),pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHAN(), SemverID(NUMBER(major), Some(NUMBER(minor+1)), Some(NUMBER(0))))))
    case SemverID(NUMBER(major), None | Some( UPPERCASEX() | LOWERCASEX() | STAR()), _,pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(0)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHAN(), SemverID(NUMBER(major+1), Some(NUMBER(0)), Some(NUMBER(0))))))


  }

}
case class TildeRange(partialRange: SemverID) extends SemverAST {
  override def transform(): SemverAST = partialRange match {
    case id@SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(patch)),pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), id), CompareRange(LESSTHAN(), SemverID(NUMBER(major), Some(NUMBER(minor+1)), Some(NUMBER(0))))))
    case SemverID(NUMBER(major), Some(NUMBER(minor)), None | Some(UPPERCASEX() | LOWERCASEX() | STAR()),pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHAN(), SemverID(NUMBER(major), Some(NUMBER(minor+1)), Some(NUMBER(0))))))
    case SemverID(NUMBER(major), None | Some( UPPERCASEX() | LOWERCASEX() | STAR()), _,pre,build) =>
      And(List(CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(0)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHAN(), SemverID(NUMBER(major+1), Some(NUMBER(0)), Some(NUMBER(0))))))

  }

}
case class HyphenRange(lhs: SemverID, rhs: SemverID) extends SemverAST {
  override def transform(): SemverAST = (lhs, rhs) match {
    case (lhs@SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(patch)), pre, build),rhs@SemverID(NUMBER(major2),Some(NUMBER(minor2)), Some(NUMBER(patch2)), pre2, build2))
    => And(List(CompareRange(GREATERTHANEQUALS(),lhs), CompareRange(LESSTHANEQUALS(),rhs)))
    case (lhs@SemverID(NUMBER(major),Some(NUMBER(minor)), None | Some(UPPERCASEX() | LOWERCASEX() | STAR()), pre, build),rhs@SemverID(NUMBER(major2),Some(NUMBER(minor2)), Some(NUMBER(patch2)), pre2, build2))
    => And(List(CompareRange(GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHANEQUALS(),rhs)))
    case (lhs@SemverID(NUMBER(major), None | Some(UPPERCASEX() | LOWERCASEX() | STAR()), _ , pre, build),rhs@SemverID(NUMBER(major2),Some(NUMBER(minor2)), Some(NUMBER(patch2)), pre2, build2))
    => And(List(CompareRange(GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(0)), Some(NUMBER(0)), pre, build)), CompareRange(LESSTHANEQUALS(),rhs)))
    case (lhs@SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(patch)), pre, build),rhs@SemverID(NUMBER(major2),Some(NUMBER(minor2)), None | Some(UPPERCASEX() | LOWERCASEX() | STAR()), pre2, build2)) =>
      And(List(CompareRange(GREATERTHANEQUALS(),lhs), CompareRange(LESSTHAN(), SemverID(NUMBER(major2),Some(NUMBER(minor2+1)), Some(NUMBER(0))))))
    case (lhs@SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(patch)), pre, build),rhs@SemverID(NUMBER(major2),None | Some(UPPERCASEX() | LOWERCASEX() | STAR()), _, pre2, build2)) =>
      And(List(CompareRange(GREATERTHANEQUALS(),lhs), CompareRange(LESSTHAN(), SemverID(NUMBER(major2+1),Some(NUMBER(0)), Some(NUMBER(0))))))

  }



}
case class CompareRange(op: SemverToken, partialRange: SemverID) extends SemverAST {
  override def transform(): SemverAST = (op, partialRange) match {
    case(_, SemverID(STAR() | UPPERCASEX() | LOWERCASEX() , _, _, _ , _)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(0), Some(NUMBER(0)), Some(NUMBER(0))))
    case(_, SemverID(STAR() | UPPERCASEX() | LOWERCASEX() , Some(STAR() | UPPERCASEX() | LOWERCASEX()), _, _ , _)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(0), Some(NUMBER(0)), Some(NUMBER(0))))
    case(_, SemverID(STAR() | UPPERCASEX() | LOWERCASEX() , Some(STAR() | UPPERCASEX() | LOWERCASEX()), Some(STAR() | UPPERCASEX() | LOWERCASEX()), _ , _)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(0), Some(NUMBER(0)), Some(NUMBER(0))))
    case (GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(UPPERCASEX() | LOWERCASEX()), pre, build)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(0)),pre,build))
    case (GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(UPPERCASEX() | LOWERCASEX()), _, pre, build)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major), Some(NUMBER(0)), Some(NUMBER(0)),pre,build))
    case (LESSTHAN(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(UPPERCASEX() | LOWERCASEX()), pre, build)) =>
      CompareRange(LESSTHAN(), SemverID(NUMBER(major), Some(NUMBER(minor)), Some(NUMBER(0)),pre,build))
    case (LESSTHAN(),SemverID(NUMBER(major),Some(UPPERCASEX() | LOWERCASEX()), _, pre, build)) =>
      CompareRange(LESSTHAN(), SemverID(NUMBER(major), Some(NUMBER(0)), Some(NUMBER(0)),pre,build))
    case (GREATERTHAN(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(UPPERCASEX() | LOWERCASEX()), pre, build)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major),Some(NUMBER(minor + 1)), Some(NUMBER(0)), pre, build))
    case (GREATERTHAN(),SemverID(NUMBER(major) ,Some(UPPERCASEX() | LOWERCASEX()), _, pre, build)) =>
      CompareRange(GREATERTHANEQUALS(), SemverID(NUMBER(major + 1),Some(NUMBER(0)), Some(NUMBER(0)), pre, build))
    case (LESSTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(UPPERCASEX() | LOWERCASEX()), pre, build)) =>
      CompareRange(LESSTHAN(), SemverID(NUMBER(major),Some(NUMBER(minor + 1)), Some(NUMBER(0)), pre, build))
    case (LESSTHANEQUALS(),SemverID(NUMBER(major) ,Some(UPPERCASEX() | LOWERCASEX()), _, pre, build)) =>
      CompareRange(LESSTHAN(), SemverID(NUMBER(major + 1),Some(NUMBER(0)), Some(NUMBER(0)), pre, build))
    case (EQUALS(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(UPPERCASEX() | LOWERCASEX()), pre, build)) =>
      And(List(CompareRange(GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(minor)), Some(NUMBER(0)), pre, build)),
        CompareRange(LESSTHAN(),SemverID(NUMBER(major),Some(NUMBER(minor+1)), Some(NUMBER(0)), pre, build))))
    case (EQUALS(),SemverID(NUMBER(major) ,Some(UPPERCASEX() | LOWERCASEX()),_ , pre, build)) =>
      And(List(CompareRange(GREATERTHANEQUALS(),SemverID(NUMBER(major),Some(NUMBER(0)), Some(NUMBER(0)), pre, build)),
        CompareRange(LESSTHAN(),SemverID(NUMBER(major + 1),Some(NUMBER(0)), Some(NUMBER(0)), pre, build))))
  }



}
case class And(rangeSet: List[SemverAST]) extends SemverAST {
  override def transform(): SemverAST = And(rangeSet.map(_.transform()))

}
case class Or(rangeSet: List[SemverAST]) extends SemverAST {
  override def transform(): SemverAST = Or(rangeSet.map(_.transform()))

}
case class SemverRange(range: SemverAST) extends SemverAST {
  override def transform: SemverAST = range.transform()

}