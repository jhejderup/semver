package io.github.jhejderup.semver

import io.github.jhejderup.semver.compiler.SemverRangeCompiler
import io.github.jhejderup.semver.lexer.{NUMBER, PREID}
import io.github.jhejderup.semver.parser.Semver

object Hello extends App {


  val range = SemverRangeCompiler("<=1.2.3").right.get.transform()
  val ver = SemverRangeCompiler("1.2.3-beta").right.get //2.4.3-alpha 1.2.3-pre.2
    .asInstanceOf[Semver]


  // <=2.4.3+asdf := <=2.4.3    2.4.3-alpha


  println(range.evaluate(ver))

}