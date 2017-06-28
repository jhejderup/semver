package io.github.jhejderup.semver.compiler

import io.github.jhejderup.semver.lexer.SemverLexer
import io.github.jhejderup.semver.parser.{SemverAST, SemverParser}

object SemverRangeCompiler {

  def clean(str: String): String = str.trim().replaceAll("^[=v]+","")

  def apply(code: String): Either[SemverCompilationError, SemverAST] = {
    for {
      tokens <- SemverLexer(clean(code)).right
      ast <- SemverParser(tokens).right
    } yield ast
  }
}