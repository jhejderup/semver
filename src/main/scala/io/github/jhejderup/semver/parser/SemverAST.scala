package io.github.jhejderup.semver.parser


import io.github.jhejderup.semver.lexer._

sealed trait SemverAST {
  def transform(): SemverAST
  def evaluate(that: Semver): Boolean
}




case class PreReleaseTags(pre:Option[List[SemverToken]] = None, build:Option[List[SemverToken]] = None)



case class Semver(major: SemverToken, minor: SemverToken, patch: SemverToken,
                  preRelease: Option[List[SemverToken]] = None,
                  metadata: Option[List[SemverToken]] = None) extends SemverAST with Ordered[Semver]  {

  override def transform(): SemverAST = this match {
    case Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build) =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build)),
        CompareRange(LT, Semver(NUMBER(major), NUMBER(minor + 1), NUMBER(0), pre, build))))
    case Semver(NUMBER(major), LETTERX | STAR, _, pre, build) if major > 0 =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build)),
        CompareRange(LT, Semver(NUMBER(major + 1), NUMBER(0), NUMBER(0), pre, build))))
    case Semver(LETTERX | STAR, LETTERX | STAR, LETTERX | STAR, pre, build)  =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(0), NUMBER(0), NUMBER(0), pre, build))))
    case _ => this
  }

  override def toString: String = {
    val preReleaseString = preRelease.map("-" + _.mkString(".")).getOrElse("")
    val metadataString = metadata.map("+" + _.mkString(".")).getOrElse("")

    s"$major.$minor.$patch" + preReleaseString + metadataString
  }

  override def evaluate(that: Semver): Boolean = that == this


  //http://semver.org/#spec-item-10
  override def compare(that: Semver): Int = (this, that) match {
    case (Semver(NUMBER(thisMajor), NUMBER(thisMinor), NUMBER(thisPatch), thisPre, _), Semver(NUMBER(thatMajor), NUMBER(thatMinor), NUMBER(thatPatch), thatPre, _)) => {
      var res = thisMajor - thatMajor
      if(res == 0) {
        res = thisMinor - thatMinor
        if (res == 0) {
          res = thisPatch - thatPatch

          if (!thisPre.isDefined && thatPre.isDefined) {
            res = 1
          } else if (thisPre.isDefined && !thatPre.isDefined) {
            res = -1
          } else {
            val thisPreList = thisPre.get
            val thatPreList = thatPre.get
            if(thisPreList.size == thatPreList.size){
              res = thisPreList.zip(thatPreList)
                .toStream
                .map(_ match {
                  case (NUMBER(t1), NUMBER(t2)) => t1.compare(t2)
                  case (PREID(t1), PREID(t2)) => t1.compare(t2)
                  case (PREID(_), NUMBER(_)) => 1
                  case (NUMBER(_), PREID(_)) => -1
                }).find(_ != 0).toList.headOption.getOrElse(0)
            } else if (thisPreList.size > thatPreList.size) {

              res = thisPreList.take(thatPreList.size).zip(thatPreList)
                .toStream
                .map(_ match {
                  case (NUMBER(t1), NUMBER(t2)) => t1.compare(t2)
                  case(PREID(t1), PREID(t2)) => t1.compare(t2)
                  case (PREID(_), NUMBER(_)) => 1
                  case (NUMBER(_), PREID(_)) => -1
                }).find(_ != 0).toList.headOption.getOrElse(-1)

            } else {
              res = thisPreList.zip(thatPreList.take(thisPreList.size))
                .toStream
                .map(_ match {
                  case (NUMBER(t1), NUMBER(t2)) => t1.compare(t2)
                  case(PREID(t1), PREID(t2)) => t1.compare(t2)
                  case (PREID(_), NUMBER(_)) => 1
                  case (NUMBER(_), PREID(_)) => -1
                }).find(_ != 0).toList.headOption.getOrElse(1)
            }
          }
        }
      }
      res
    }
  }
}


case class CaretRange(partialRange: Semver) extends SemverAST {
  override def transform(): SemverAST = partialRange match {
    case id@Semver(NUMBER(0), NUMBER(0), NUMBER(patch), _, _) if patch > 0 =>
      Intersection(List(CompareRange(GTE, id),
        CompareRange(LT,Semver(NUMBER(0), NUMBER(0), NUMBER(patch+1)))))
    case id@Semver(NUMBER(0), NUMBER(minor), NUMBER(_), _, _) if minor > 0 =>
      Intersection(List(CompareRange(GTE, id),
        CompareRange(LT,Semver(NUMBER(0), NUMBER(minor+1), NUMBER(0)))))
    case id@Semver(NUMBER(major), NUMBER(_), NUMBER(_), _, _) if major > 0 =>
      Intersection(List(CompareRange(GTE, id),
        CompareRange(LT,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0)))))
    case Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build) if major > 0 =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build)),
        CompareRange(LT,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0)))))
    case Semver(NUMBER(0), NUMBER(minor), LETTERX | STAR, pre, build)  =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(0), NUMBER(minor), NUMBER(0), pre, build)),
        CompareRange(LT,Semver(NUMBER(0), NUMBER(minor+1), NUMBER(0)))))
    case Semver(NUMBER(major), LETTERX | STAR, _, pre, build)  =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build)),
        CompareRange(LT,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0)))))
  }
  override def toString: String = s"^$partialRange"
  override def evaluate(that: Semver): Boolean = this.transform().evaluate(that)
}
case class TildeRange(partialRange: Semver) extends SemverAST {
  override def transform(): SemverAST = partialRange match {
    case id@Semver(NUMBER(major), NUMBER(minor), NUMBER(_), _, _) =>
      Intersection(List(CompareRange(GTE,id),
        CompareRange(LT, Semver(NUMBER(major), NUMBER(minor+1), NUMBER(0)))))
    case Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre , build) =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build)),
        CompareRange(LT,Semver(NUMBER(major), NUMBER(minor +1), NUMBER(0)) )))
    case Semver(NUMBER(major), LETTERX | STAR, _, pre , build) =>
      Intersection(List(CompareRange(GTE, Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build)),
      CompareRange(LT,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0)) )))
  }

  override def toString: String = s"~$partialRange"
  override def evaluate(that: Semver): Boolean = this.transform().evaluate(that)
}
case class HyphenRange(lhs: Semver, rhs: Semver) extends SemverAST {
  override def transform(): SemverAST = {
    val desugerlhs = lhs match {
      case Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre , build) =>
        CompareRange(GTE, Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build))
      case Semver(NUMBER(major), LETTERX | STAR, _, pre , build) =>
        CompareRange(GTE, Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build))
      case id@Semver(NUMBER(_), NUMBER(_), NUMBER(_), _ ,_) =>
        CompareRange(GTE, id)
    }
    val desugerrhs = rhs match {
      case Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre , build) =>
        CompareRange(LT, Semver(NUMBER(major), NUMBER(minor+1), NUMBER(0), pre, build))
      case Semver(NUMBER(major), LETTERX | STAR, _, pre , build) =>
        CompareRange(LT, Semver(NUMBER(major+1), NUMBER(0), NUMBER(0), pre, build))
      case id@Semver(NUMBER(_), NUMBER(_), NUMBER(_), _ ,_) =>
        CompareRange(LTE, id)

    }
    Intersection(List(desugerlhs, desugerrhs))
  }

  override def toString: String = s"$lhs - $rhs"

  override def evaluate(that: Semver): Boolean = this.transform().evaluate(that)
}
case class CompareRange(op: SemverToken, partialRange: Semver) extends SemverAST {
  override def transform(): SemverAST = (op, partialRange) match {
    case (EQU, id@Semver(_, _, _, _, _)) => id.transform()
    case (LTE, Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build)) =>
      CompareRange(LT,Semver(NUMBER(major), NUMBER(minor+1), NUMBER(0), pre, build))
    case (LTE, Semver(NUMBER(major), LETTERX | STAR, _, pre, build)) =>
      CompareRange(LT,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0), pre, build))
    case (GT, Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build)) =>
      CompareRange(GTE,Semver(NUMBER(major), NUMBER(minor+1), NUMBER(0), pre, build))
    case (GT, Semver(NUMBER(major), LETTERX | STAR, _, pre, build)) =>
      CompareRange(GTE,Semver(NUMBER(major+1), NUMBER(0), NUMBER(0), pre, build))
    case (LT, Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build)) =>
      CompareRange(LT,Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build))
    case (LT, Semver(NUMBER(major), LETTERX | STAR, _, pre, build)) =>
      CompareRange(LT,Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build))
    case (GTE, Semver(NUMBER(major), NUMBER(minor), LETTERX | STAR, pre, build)) =>
      CompareRange(GTE,Semver(NUMBER(major), NUMBER(minor), NUMBER(0), pre, build))
    case (GTE, Semver(NUMBER(major), LETTERX | STAR, _, pre, build)) =>
      CompareRange(GTE,Semver(NUMBER(major), NUMBER(0), NUMBER(0), pre, build))
    case (_, Semver(LETTERX | STAR, _, _, _, _)) =>
      CompareRange(GTE,Semver(NUMBER(0), NUMBER(0), NUMBER(0)))
    case (_,_) => this
  }

  override def toString: String = s"$op$partialRange"

  override def evaluate(that: Semver): Boolean = op match {
    case EQU => that == partialRange
    case LT => that < partialRange
    case GT => that > partialRange
    case LTE => that <= partialRange
    case GTE => that >= partialRange
  }

}
case class Intersection(rangeSet: List[SemverAST]) extends SemverAST {
  override def transform(): SemverAST = Intersection(rangeSet.map(r => r.transform()))
  override def toString: String = rangeSet.mkString(" ")
  override def evaluate(that: Semver): Boolean = rangeSet.map(r => r.evaluate(that)).reduceOption(_ && _).getOrElse(false)

}
case class Union(rangeSet: List[SemverAST]) extends SemverAST {
  override def transform(): SemverAST = Union(rangeSet.map(r => r.transform()))

  override def toString: String = rangeSet.mkString(UNION.toString)

  override def evaluate(that: Semver): Boolean = rangeSet.map(r => r.evaluate(that)).reduceOption(_ || _).getOrElse(false)

}
