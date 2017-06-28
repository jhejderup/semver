package io.github.jhejderup.semver

import io.github.jhejderup.semver.compiler.SemverRangeCompiler
import io.github.jhejderup.semver.lexer.{NUMBER, PREID}
import io.github.jhejderup.semver.parser.Semver

object Hello extends App {
  val ast = SemverRangeCompiler("||")
  println(ast)
//  println(ast.right.get)
//  println(ast.right.get.transform())
 // println(ast.right.get.transform().evaluate(Semver(NUMBER(1), NUMBER(2), NUMBER(3),Some(List(PREID("pre"), NUMBER(2), NUMBER(2))))))
}
