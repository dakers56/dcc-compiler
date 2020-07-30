package com.dakers.compiler

/**
 * Trait representing a Lexer for a compiler. Breaks source code into tokens.
 */
trait Lexer {
  /**
   * @param s source code to lex
   * @return [[Seq]] of tokens in order encountered in source code
   */
  def lex(s : String) : Seq[String]
}
