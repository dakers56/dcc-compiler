package com.dakers.compiler

import scala.util.matching.Regex

/**
 * Trait representing a Lexer for a compiler. Breaks source code into tokens.
 */
trait Lexer {
  /**
   * @param s source code to tokenize
   * @return [[Seq]] of tokens in order encountered in source code
   */
  def tokenize(s: String): Seq[Token]

}

/**
 * Smallest unit a parser can understand
 *
 * @param s String representation of this token in source code
 */
case class Token(s: String)

/**
 * Regex that a token must conform to. Private constructor to ensure that the [[Regex]] is correctly constructed using
 * the companion object.
 *
 * @param re regex defining the token
 */
class TokenRegex private(val re: Regex)

/**
 * Contains functionality to initialize [[TokenRegex]] properly.
 */
object TokenRegex {
  /**
   * @param l list of string regexes
   * @return [[TokenRegex]] comprised of the elements of l turned into capturing groups, and OR'd together
   */
  def apply(l: List[String]): TokenRegex = {
    new TokenRegex(("^(" + (l.head + l.tail.foldLeft("| " + _)) + ")*$").r)
  }
}

/**
 * Tokenizes source code according to given [[TokenRegex]]
 *
 * @param t See [[TokenRegex]]
 */
class TokenRegexLexer(t: TokenRegex) extends Lexer {
  /**
   * @param s source code to tokenize
   * @return [[Seq]] of tokens in order encountered in source code
   */
  override def tokenize(s: String): Seq[Token] = {
    t.re.findAllMatchIn(s).map(_.toString()).map(s => Token(s)).toSeq
  }
}
