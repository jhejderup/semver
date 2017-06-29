package io.github.jhejderup.semver

import io.github.jhejderup.semver.compiler.SemverRangeCompiler
import io.github.jhejderup.semver.lexer.{NUMBER, PREID}
import io.github.jhejderup.semver.parser.Semver

object Hello extends App {


  val range = SemverRangeCompiler("<1.2.3").right.get.transform()
  val ver = SemverRangeCompiler("1.2.3").right.get
    .asInstanceOf[Semver]


  println(range.evaluate(ver))

}