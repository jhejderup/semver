package io.github.jhejderup.semver.compiler


sealed trait SemverCompilationError

case class SemverLexerError(location: Location, msg: String) extends SemverCompilationError

case class SemverParserError(location: Location, msg: String) extends SemverCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}