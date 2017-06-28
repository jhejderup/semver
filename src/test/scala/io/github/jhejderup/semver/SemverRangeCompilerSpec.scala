package io.github.jhejderup.semver.compiler

import org.scalatest._

class SemverRangeCompilerSpec extends FlatSpec with Matchers {
  "The SemverRangeCompiler object" should "equal true" in {
    //https://github.com/npm/node-semver/blob/master/test/index.js#L459
    val validRangeTest = List[(String, String)](
      ("1.0.0 - 2.0.0", ">=1.0.0 <=2.0.0"),
      ("1.0.0", "1.0.0"),
//      (">=*", "*")
//      ("", "*"),
//      ("*", "*"),
//      ("*", "*"),
      (">=1.0.0", ">=1.0.0"),
      (">1.0.0", ">1.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("1", ">=1.0.0 <2.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("<2.0.0", "<2.0.0"),
      ("<2.0.0", "<2.0.0"),
      (">= 1.0.0", ">=1.0.0"),
//      (">=  1.0.0", ">=1.0.0"),
//      (">=   1.0.0", ">=1.0.0"),
//      ("> 1.0.0", ">1.0.0"),
//      (">  1.0.0", ">1.0.0"),
//      ("<=   2.0.0", "<=2.0.0"),
//      ("<= 2.0.0", "<=2.0.0"),
//      ("<=  2.0.0", "<=2.0.0"),
//      ("<    2.0.0", "<2.0.0"),
//      ("<	2.0.0", "<2.0.0"),
     (">=0.1.97", ">=0.1.97"),
      (">=0.1.97", ">=0.1.97"),
      ("0.1.20 || 1.2.4", "0.1.20||1.2.4"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
//      ("||", "||"),
      ("2.x.x", ">=2.0.0 <3.0.0"),
      ("1.2.x", ">=1.2.0 <1.3.0"),
      ("1.2.x || 2.x", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
      ("1.2.x || 2.x", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
//      ("x", "*"),
      ("2.*.*", ">=2.0.0 <3.0.0"),
      ("1.2.*", ">=1.2.0 <1.3.0"),
      ("1.2.* || 2.*", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
//      ("*", "*"),
      ("2", ">=2.0.0 <3.0.0"),
      ("2.3", ">=2.3.0 <2.4.0"),
      ("~2.4", ">=2.4.0 <2.5.0"),
      ("~2.4", ">=2.4.0 <2.5.0"),
//      ("~>3.2.1", ">=3.2.1 <3.3.0"),
      ("~1", ">=1.0.0 <2.0.0"),
//      ("~>1", ">=1.0.0 <2.0.0"),
//      ("~> 1", ">=1.0.0 <2.0.0"),
      ("~1.0", ">=1.0.0 <1.1.0"),
//      ("~ 1.0", ">=1.0.0 <1.1.0"),
      ("^0", ">=0.0.0 <1.0.0"),
//      ("^ 1", ">=1.0.0 <2.0.0"),
      ("^0.1", ">=0.1.0 <0.2.0"),
      ("^1.0", ">=1.0.0 <2.0.0"),
      ("^1.2", ">=1.2.0 <2.0.0"),
      ("^0.0.1", ">=0.0.1 <0.0.2"),
      ("^0.0.1-beta", ">=0.0.1-beta <0.0.2"),
      ("^0.1.2", ">=0.1.2 <0.2.0"),
      ("^1.2.3", ">=1.2.3 <2.0.0"),
      ("^1.2.3-beta.4", ">=1.2.3-beta.4 <2.0.0"),
      ("<1", "<1.0.0"),
//      ("< 1", "<1.0.0"),
      (">=1", ">=1.0.0"),
//      (">= 1", ">=1.0.0"),
      ("<1.2", "<1.2.0"),
//      ("< 1.2", "<1.2.0"),
      ("1", ">=1.0.0 <2.0.0"),
//    //  (">01.02.03", ">1.2.3", true),
//    //  (">01.02.03", null),
//      ("~1.2.3beta", ">=1.2.3-beta <1.3.0"),
//    //  ("~1.2.3beta", null),
//      ("^ 1.2 ^ 1", ">=1.2.0 <2.0.0 >=1.0.0 <2.0.0")
    )


    for {
      range <- validRangeTest
    } yield SemverRangeCompiler(range._1).right.get.transform().toString should equal(range._2)

  }
}
