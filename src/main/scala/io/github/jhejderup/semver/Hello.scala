package io.github.jhejderup.semver

import io.github.jhejderup.semver.compiler.SemverRangeCompiler


object Hello extends App {
  val ast = SemverRangeCompiler("")
  println(ast.right.get)
  println(ast.right.get.transform())
}
