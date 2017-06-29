package io.github.jhejderup.semver.compiler

import io.github.jhejderup.semver.parser.{Semver, SemverAST}
import org.scalatest._

class SemverRangeCompilerSpec extends FlatSpec with Matchers {
  "The SemverRangeCompiler transform method" should "equal true" in {
    //https://github.com/npm/node-semver/blob/master/test/index.js#L459
    val validRangeTest = List[(String, String)](
      ("1.0.0 - 2.0.0", ">=1.0.0 <=2.0.0"),
      ("1.0.0", "1.0.0"),
      (">=*", ">=0.0.0"), // (">=*", "*"), // we evaluate any op + * as * e.g <* := * https://github.com/npm/node-semver/blob/d21444a0658224b152ce54965d02dbe0856afb84/semver.js#L225
      ("", ">=0.0.0"), //  ("", "*"),
      ("*", ">=0.0.0"), // ("*", "*"),
//      ("*", "*"),      //duplicate!!
      (">=1.0.0", ">=1.0.0"),
      (">1.0.0", ">1.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("1", ">=1.0.0 <2.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("<=2.0.0", "<=2.0.0"),
      ("<2.0.0", "<2.0.0"),
      ("<2.0.0", "<2.0.0"),
      (">= 1.0.0", ">=1.0.0"),
      (">=  1.0.0", ">=1.0.0"),
      (">=   1.0.0", ">=1.0.0"),
      ("> 1.0.0", ">1.0.0"),
      (">  1.0.0", ">1.0.0"),
      ("<=   2.0.0", "<=2.0.0"),
      ("<= 2.0.0", "<=2.0.0"),
      ("<=  2.0.0", "<=2.0.0"),
      ("<    2.0.0", "<2.0.0"),
      ("< 2.0.0", "<2.0.0"),
      (">=0.1.97", ">=0.1.97"),
      (">=0.1.97", ">=0.1.97"),
      ("0.1.20 || 1.2.4", "0.1.20||1.2.4"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
      (">=0.2.3 || <0.0.1", ">=0.2.3||<0.0.1"),
      ("||", ">=0.0.0"), // ("||", "||")
      ("2.x.x", ">=2.0.0 <3.0.0"),
      ("1.2.x", ">=1.2.0 <1.3.0"),
      ("1.2.x || 2.x", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
      ("1.2.x || 2.x", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
      ("x", ">=0.0.0"), //      ("x", "*"),
      ("2.*.*", ">=2.0.0 <3.0.0"),
      ("1.2.*", ">=1.2.0 <1.3.0"),
      ("1.2.* || 2.*", ">=1.2.0 <1.3.0||>=2.0.0 <3.0.0"),
//      ("*", "*"), // duplicate!
      ("2", ">=2.0.0 <3.0.0"),
      ("2.3", ">=2.3.0 <2.4.0"),
      ("~2.4", ">=2.4.0 <2.5.0"),
      ("~2.4", ">=2.4.0 <2.5.0"),
      ("~>3.2.1", ">=3.2.1 <3.3.0"),
      ("~1", ">=1.0.0 <2.0.0"),
      ("~>1", ">=1.0.0 <2.0.0"),
      ("~> 1", ">=1.0.0 <2.0.0"),
      ("~1.0", ">=1.0.0 <1.1.0"),
      ("~ 1.0", ">=1.0.0 <1.1.0"),
      ("^0", ">=0.0.0 <1.0.0"),
      ("^ 1", ">=1.0.0 <2.0.0"),
      ("^0.1", ">=0.1.0 <0.2.0"),
      ("^1.0", ">=1.0.0 <2.0.0"),
      ("^1.2", ">=1.2.0 <2.0.0"),
      ("^0.0.1", ">=0.0.1 <0.0.2"),
      ("^0.0.1-beta", ">=0.0.1-beta <0.0.2"),
      ("^0.1.2", ">=0.1.2 <0.2.0"),
      ("^1.2.3", ">=1.2.3 <2.0.0"),
      ("^1.2.3-beta.4", ">=1.2.3-beta.4 <2.0.0"),
      ("<1", "<1.0.0"),
      ("< 1", "<1.0.0"),
      (">=1", ">=1.0.0"),
      (">= 1", ">=1.0.0"),
      ("<1.2", "<1.2.0"),
      ("< 1.2", "<1.2.0"),
      ("1", ">=1.0.0 <2.0.0"),
//    //  (">01.02.03", ">1.2.3"/*, true*/),  --- this for loose case, not supported yet
//    //  (">01.02.03", null),  --- this for loose case, not supported yet
//      ("~1.2.3beta", ">=1.2.3-beta <1.3.0"/*, true*/), --- this for loose case, not supported yet
//    //  ("~1.2.3beta", null),  --- this for loose case, not supported yet
      ("^ 1.2 ^ 1", ">=1.2.0 <2.0.0 >=1.0.0 <2.0.0")
    )

    for {
      range <- validRangeTest
    } yield
      SemverRangeCompiler(range._1).right.get
        .transform()
        .toString should equal(range._2)

  }

  "The Semver object comparison" should "equal true" in {

    // version1 should be greater than version2
    val comparisonTest = List[(String, String)](
      ("0.0.0", "0.0.0-foo"),
      ("0.0.1", "0.0.0"),
      ("1.0.0", "0.9.9"),
      ("0.10.0", "0.9.0"),
      ("0.99.0", "0.10.0"),
      ("2.0.0", "1.2.3"),
      ("v0.0.0", "0.0.0-foo" /*, true*/ ),
      ("v0.0.1", "0.0.0" /*, true*/ ),
      ("v1.0.0", "0.9.9" /*, true*/ ),
      ("v0.10.0", "0.9.0" /*, true*/ ),
      ("v0.99.0", "0.10.0" /*, true*/ ),
      ("v2.0.0", "1.2.3" /*, true*/ ),
      ("0.0.0", "v0.0.0-foo" /*, true*/ ),
      ("0.0.1", "v0.0.0" /*, true*/ ),
      ("1.0.0", "v0.9.9" /*, true*/ ),
      ("0.10.0", "v0.9.0" /*, true*/ ),
      ("0.99.0", "v0.10.0" /*, true*/ ),
      ("2.0.0", "v1.2.3" /*, true*/ ),
      ("1.2.3", "1.2.3-asdf"),
      ("1.2.3", "1.2.3-4"),
      ("1.2.3", "1.2.3-4-foo"),
      ("1.2.3-5-foo", "1.2.3-5"),
      ("1.2.3-5", "1.2.3-4"),
      ("1.2.3-5-foo", "1.2.3-5-Foo"),
      ("3.0.0", "2.7.2+asdf"),
      ("1.2.3-a.10", "1.2.3-a.5"),
      ("1.2.3-a.b", "1.2.3-a.5"),
      ("1.2.3-a.b", "1.2.3-a"),
      ("1.2.3-a.b.c.10.d.5", "1.2.3-a.b.c.5.d.100"),
      ("1.2.3-r2", "1.2.3-r100"),
      ("1.2.3-r100", "1.2.3-R2"),
      ("1.0.0-alpha.1", "1.0.0-alpha")
    )

    val res = for {
      cmp <- comparisonTest
    } yield {
      val v0 = SemverRangeCompiler(cmp._1).right.get
        .asInstanceOf[Semver]
      val v1 = SemverRangeCompiler(cmp._2).right.get
        .asInstanceOf[Semver]

      v0 > v1 && v1 < v0 && !(v1 > v0) && !(v0 < v1) && v0 == v0 && v1 == v1 && v0 != v1 && v0 >= v1 && v1 <= v0
    }
    res.reduceOption(_ && _).getOrElse(false) should be(true)
  }

  "The Semver object comparison checking equality" should "equal true" in {

    // version1 should be greater than version2
    val comparisonTest = List[(String, String)](
      ("1.2.3", "v1.2.3"),
      ("1.2.3", "=1.2.3" /*, true */ ),
      ("1.2.3", "v 1.2.3" /*, true */ ),
      ("1.2.3", "= 1.2.3" /*, true */ ),
      ("1.2.3", " v1.2.3" /*, true */ ),
      ("1.2.3", " =1.2.3" /*, true */ ),
      ("1.2.3", " v 1.2.3" /*, true */ ),
      ("1.2.3", " = 1.2.3" /*, true */ ),
      ("1.2.3-0", "v1.2.3-0" /*, true */ ),
      ("1.2.3-0", "=1.2.3-0" /*, true */ ),
      ("1.2.3-0", "v 1.2.3-0" /*, true */ ),
      ("1.2.3-0", "= 1.2.3-0" /*, true */ ),
      ("1.2.3-0", " v1.2.3-0" /*, true */ ),
      ("1.2.3-0", " =1.2.3-0" /*, true */ ),
      ("1.2.3-0", " v 1.2.3-0" /*, true */ ),
      ("1.2.3-0", " = 1.2.3-0" /*, true */ ),
      ("1.2.3-1", "v1.2.3-1" /*, true */ ),
      ("1.2.3-1", "=1.2.3-1" /*, true */ ),
      ("1.2.3-1", "v 1.2.3-1" /*, true */ ),
      ("1.2.3-1", "= 1.2.3-1" /*, true */ ),
      ("1.2.3-1", " v1.2.3-1" /*, true */ ),
      ("1.2.3-1", " =1.2.3-1" /*, true */ ),
      ("1.2.3-1", " v 1.2.3-1" /*, true */ ),
      ("1.2.3-1", " = 1.2.3-1" /*, true */ ),
      ("1.2.3-beta", "v1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", "=1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", "v 1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", "= 1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", " v1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", " =1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", " v 1.2.3-beta" /*, true */ ),
      ("1.2.3-beta", " = 1.2.3-beta" /*, true */ ),
      ("1.2.3-beta+build", " = 1.2.3-beta+otherbuild" /*, true */ ),
      ("1.2.3+build", " = 1.2.3+otherbuild" /*, true */ ),
      ("1.2.3-beta+build", "1.2.3-beta+otherbuild"),
      ("1.2.3+build", "1.2.3+otherbuild"),
      ("  v1.2.3+build", "1.2.3+otherbuild")
    )

    val res = for {
      cmp <- comparisonTest
    } yield {
      val v0 = SemverRangeCompiler(cmp._1).right.get
        .asInstanceOf[Semver]
      val v1 = SemverRangeCompiler(cmp._2).right.get
        .asInstanceOf[Semver]

      v0 == v1 && !(v1 != v0) && !(v0 != v1) && v1 == v0 && !(v0 > v1) && !(v0 < v1) && v0 >= v1 && v0 <= v1

    }
    res.reduceOption(_ && _).getOrElse(false) should be(true)
  }

  "The Semver objectvalid range check" should "equal true" in {

    // version should be included by range
    val validRangeTest = List[(String, String)](
      ("1.0.0 - 2.0.0", "1.2.3"),
      ("^1.2.3+build", "1.2.3"),
      ("^1.2.3+build", "1.3.0"),
      ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3"),
   //   ("1.2.3pre+asdf - 2.4.3-pre+asdf", "1.2.3", true),  --- this for loose case, not supported yet
   //   ("1.2.3-pre+asdf - 2.4.3pre+asdf", "1.2.3", true),  --- this for loose case, not supported yet
   //   ("1.2.3pre+asdf - 2.4.3pre+asdf", "1.2.3", true),  --- this for loose case, not supported yet
      ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3-pre.2"),
      ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "2.4.3-alpha"),
      ("1.2.3+asdf - 2.4.3+asdf", "1.2.3"),
      ("1.0.0", "1.0.0"),
      (">=*", "0.2.4"),
      ("", "1.0.0"),
      ("*", "1.2.3"),
      ("*", "v1.2.3"/*, true */),
      (">=1.0.0", "1.0.0"),
      (">=1.0.0", "1.0.1"),
      (">=1.0.0", "1.1.0"),
      (">1.0.0", "1.0.1"),
      (">1.0.0", "1.1.0"),
      ("<=2.0.0", "2.0.0"),
      ("<=2.0.0", "1.9999.9999"),
      ("<=2.0.0", "0.2.9"),
      ("<2.0.0", "1.9999.9999"),
      ("<2.0.0", "0.2.9"),
      (">= 1.0.0", "1.0.0"),
      (">=  1.0.0", "1.0.1"),
      (">=   1.0.0", "1.1.0"),
      ("> 1.0.0", "1.0.1"),
      (">  1.0.0", "1.1.0"),
      ("<=   2.0.0", "2.0.0"),
      ("<= 2.0.0", "1.9999.9999"),
      ("<=  2.0.0", "0.2.9"),
      ("<    2.0.0", "1.9999.9999"),
   //   ("<\t2.0.0", "0.2.9"), need help with this form of escaping
      (">=0.1.97", "v0.1.97"/*, true */),
      (">=0.1.97", "0.1.97"),
      ("0.1.20 || 1.2.4", "1.2.4"),
      (">=0.2.3 || <0.0.1", "0.0.0"),
      (">=0.2.3 || <0.0.1", "0.2.3"),
      (">=0.2.3 || <0.0.1", "0.2.4"),
      ("||", "1.3.4"),
      ("2.x.x", "2.1.3"),
      ("1.2.x", "1.2.3"),
      ("1.2.x || 2.x", "2.1.3"),
      ("1.2.x || 2.x", "1.2.3"),
      ("x", "1.2.3"),
      ("2.*.*", "2.1.3"),
      ("1.2.*", "1.2.3"),
      ("1.2.* || 2.*", "2.1.3"),
      ("1.2.* || 2.*", "1.2.3"),
      ("*", "1.2.3"),
      ("2", "2.1.2"),
      ("2.3", "2.3.1"),
      ("~2.4", "2.4.0"), // >=2.4.0 <2.5.0
      ("~2.4", "2.4.5"),
      ("~>3.2.1", "3.2.2"), // >=3.2.1 <3.3.0,
      ("~1", "1.2.3"), // >=1.0.0 <2.0.0
      ("~>1", "1.2.3"),
      ("~> 1", "1.2.3"),
      ("~1.0", "1.0.2"), // >=1.0.0 <1.1.0,
      ("~ 1.0", "1.0.2"),
      ("~ 1.0.3", "1.0.12"),
      (">=1", "1.0.0"),
      (">= 1", "1.0.0"),
      ("<1.2", "1.1.1"),
      ("< 1.2", "1.1.1"),
      ("~0.5.4-pre", "0.5.5"),
      ("~v0.5.4-pre", "0.5.4"),
      ("=0.7.x", "0.7.2"),
      ("<=0.7.x", "0.7.2"),
      (">=0.7.x", "0.7.2"),
      ("<=0.7.x", "0.6.2"),
      ("~1.2.1 >=1.2.3", "1.2.3"),
      ("~1.2.1 =1.2.3", "1.2.3"),
      ("~1.2.1 1.2.3", "1.2.3"),
      ("~1.2.1 >=1.2.3 1.2.3", "1.2.3"),
      ("~1.2.1 1.2.3 >=1.2.3", "1.2.3"),
      ("~1.2.1 1.2.3", "1.2.3"),
      (">=1.2.1 1.2.3", "1.2.3"),
      ("1.2.3 >=1.2.1", "1.2.3"),
      (">=1.2.3 >=1.2.1", "1.2.3"),
      (">=1.2.1 >=1.2.3", "1.2.3"),
      (">=1.2", "1.2.8"),
      ("^1.2.3", "1.8.1"),
      ("^0.1.2", "0.1.2"),
      ("^0.1", "0.1.2"),
      ("^1.2", "1.4.2"),
      ("^1.2 ^1", "1.4.2"),
      ("^1.2.3-alpha", "1.2.3-pre"),
      ("^1.2.0-alpha", "1.2.0-pre"),
      ("^0.0.1-alpha", "0.0.1-beta")
    )

    val res = for {
      cmp <- validRangeTest
    } yield {
      val range = SemverRangeCompiler(cmp._1).right.get.transform()
      val ver = SemverRangeCompiler(cmp._2).right.get
        .asInstanceOf[Semver]

      range.evaluate(ver)
    }
    res.reduceOption(_ && _).getOrElse(false) should be(true)
  }

  "The Semver object invalid range check" should "equal false" in {

    // version should be included by range
    val invalidRangeTest = List[(String, String)](
      ("1.0.0 - 2.0.0", "2.2.3"),
      ("1.2.3+asdf - 2.4.3+asdf", "1.2.3-pre.2"),
    //  ("1.2.3+asdf - 2.4.3+asdf", "2.4.3-alpha"), //-> have to check how npm treat, diff from semver spec
      ("^1.2.3+build", "2.0.0"),
      ("^1.2.3+build", "1.2.0"),
      ("^1.2.3", "1.2.3-pre"),
      ("^1.2", "1.2.0-pre"),
      (">1.2", "1.3.0-beta"),
  //    ("<=1.2.3", "1.2.3-beta"), //-> have to check how npm treat, diff from semver spec
      ("^1.2.3", "1.2.3-beta"),
      ("=0.7.x", "0.7.0-asdf"),
      (">=0.7.x", "0.7.0-asdf"),
//  //    ("1", "1.0.0beta", true),  --- this for loose case, not supported yet
//  //    ("<1", "1.0.0beta", true),  --- this for loose case, not supported yet
//  //    ("< 1", "1.0.0beta", true),  --- this for loose case, not supported yet
      ("1.0.0", "1.0.1"),
      (">=1.0.0", "0.0.0"),
      (">=1.0.0", "0.0.1"),
      (">=1.0.0", "0.1.0"),
      (">1.0.0", "0.0.1"),
      (">1.0.0", "0.1.0"),
      ("<=2.0.0", "3.0.0"),
      ("<=2.0.0", "2.9999.9999"),
      ("<=2.0.0", "2.2.9"),
      ("<2.0.0", "2.9999.9999"),
      ("<2.0.0", "2.2.9"),
      (">=0.1.97", "v0.1.93"/*, true */),
      (">=0.1.97", "0.1.93"),
      ("0.1.20 || 1.2.4", "1.2.3"),
      (">=0.2.3 || <0.0.1", "0.0.3"),
      (">=0.2.3 || <0.0.1", "0.2.2"),
      ("2.x.x", "1.1.3"),
      ("2.x.x", "3.1.3"),
      ("1.2.x", "1.3.3"),
      ("1.2.x || 2.x", "3.1.3"),
      ("1.2.x || 2.x", "1.1.3"),
      ("2.*.*", "1.1.3"),
      ("2.*.*", "3.1.3"),
      ("1.2.*", "1.3.3"),
      ("1.2.* || 2.*", "3.1.3"),
      ("1.2.* || 2.*", "1.1.3"),
      ("2", "1.1.2"),
      ("2.3", "2.4.1"),
      ("~2.4", "2.5.0"), // >=2.4.0 <2.5.0
      ("~2.4", "2.3.9"),
      ("~>3.2.1", "3.3.2"), // >=3.2.1 <3.3.0
      ("~>3.2.1", "3.2.0"), // >=3.2.1 <3.3.0
      ("~1", "0.2.3"), // >=1.0.0 <2.0.0
      ("~>1", "2.2.3"),
      ("~1.0", "1.1.0"), // >=1.0.0 <1.1.0
      ("<1", "1.0.0"),
      (">=1.2", "1.1.1"),
//    //  ("1", "2.0.0beta", true),  --- this for loose case, not supported yet
      ("~v0.5.4-beta", "0.5.4-alpha"),
      ("=0.7.x", "0.8.2"),
      (">=0.7.x", "0.6.2"),
      ("<0.7.x", "0.7.2"),
//      ("<1.2.3", "1.2.3-beta"), //  //-> have to check how npm treat, diff from semver spec
      ("=1.2.3", "1.2.3-beta"),
      (">1.2", "1.2.8"),
//      ("^1.2.3", "2.0.0-alpha"),  //  //-> have to check how npm treat, diff from semver spec
      ("^1.2.3", "1.2.2"),
      ("^1.2", "1.1.9"),
 //     ("*", "v1.2.3-foo" /*, true*/),  //  //-> have to check how npm treat, diff from semver spec
//      // invalid ranges never satisfied!
//  //    ("blerg", "1.2.3"),
//  //    ("git+https://user:password0123@github.com/foo", "123.0.0", true),
//      ("^1.2.3", "2.0.0-pre") //  we have to check here //-> have to check how npm treat, diff from semver spec
    )

    val res = for {
      cmp <- invalidRangeTest
    } yield {
      val range = SemverRangeCompiler(cmp._1).right.get.transform()
      val ver = SemverRangeCompiler(cmp._2).right.get
        .asInstanceOf[Semver]

      range.evaluate(ver)
    }
    res.reduceOption(_ || _).getOrElse(true) should be(false)
  }

  /* -->> need to have some better handling for this
  test('\ninvalid version numbers', function(t) {
  ['1.2.3.4',
   'NOT VALID',
   1.2,
   null,
   'Infinity.NaN.Infinity'
  ].forEach(function(v) {
    t.throws(function() {
      new SemVer(v);
    }, {name:'TypeError', message:'Invalid Version: ' + v});
  });

  t.end();
});
  * */


  "The Semver object maxSatisying" should "equal true" in {
    val t1 = new Tuple3[List[String], String, String](List[String]("1.2.3", "1.2.4"), "1.2", "1.2.4")
    val t2 = new Tuple3[List[String], String, String](List[String]("1.2.4", "1.2.3"), "1.2", "1.2.4")
    val t3 = new Tuple3[List[String], String, String](List[String]("1.2.3", "1.2.4", "1.2.5", "1.2.6"), "~1.2.3", "1.2.6")
    val t4 = new Tuple3[List[String], String, String](List[String]("1.1.0", "1.2.0", "1.2.1", "1.3.0", "2.0.0-b1", "2.0.0-b2", "2.0.0-b3", "2.0.0", "2.1.0"), "~2.0.0", "2.0.0") //change later to original
    //t4 should be [["1.1.0", "1.2.0", "1.2.1", "1.3.0", "2.0.0-b1", "2.0.0-b2", "2.0.0-b3", "2.0.0", "2.1.0"], '~2.0.0', '2.0.0', true]


    def semver(str: String): Semver = SemverRangeCompiler(str).right.get.asInstanceOf[Semver]
    def range(str: String): SemverAST =  SemverRangeCompiler(str).right.get.transform()

    val res = for {
      cmp <- List(t1,t2,t3, t4)
    } yield {
      val r = range(cmp._2)
      val f = semver(cmp._3)
      val max = cmp._1.map(v => semver(v)).filter(v => r.evaluate(v)).max
      max == f

    }


    res.reduceOption(_ && _).getOrElse(false) should be(true)


  }

  "The Semver object minSatisying" should "equal true" in {
    val t1 = new Tuple3[List[String], String, String](List[String]("1.2.3", "1.2.4"), "1.2", "1.2.3")
    val t2 = new Tuple3[List[String], String, String](List[String]("1.2.4", "1.2.3"), "1.2", "1.2.3")
    val t3 = new Tuple3[List[String], String, String](List[String]("1.2.3", "1.2.4", "1.2.5", "1.2.6"), "~1.2.3", "1.2.3")
    val t4 = new Tuple3[List[String], String, String](List[String]("1.1.0", "1.2.0", "1.2.1", "1.3.0", "2.0.0-b1", "2.0.0-b2", "2.0.0-b3", "2.0.0", "2.1.0"), "~2.0.0", "2.0.0") //change later to original
    //t4 should be [["1.1.0", "1.2.0", "1.2.1", "1.3.0", "2.0.0-b1", "2.0.0-b2", "2.0.0-b3", "2.0.0", "2.1.0"], '~2.0.0', '2.0.0', true]


    def semver(str: String): Semver = SemverRangeCompiler(str).right.get.asInstanceOf[Semver]
    def range(str: String): SemverAST =  SemverRangeCompiler(str).right.get.transform()

    val res = for {
      cmp <- List(t1,t2,t3, t4)
    } yield {
      val r = range(cmp._2)
      val f = semver(cmp._3)
      val min = cmp._1.map(v => semver(v)).filter(v => r.evaluate(v)).min

      min == f

    }

    res.reduceOption(_ && _).getOrElse(false) should be(true)


  }
}
