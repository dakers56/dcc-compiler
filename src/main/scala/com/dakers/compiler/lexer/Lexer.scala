package com.dakers.compiler.lexer

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
case class Token(s: String) {
  override def toString: String = "\"" + s + "\""
}

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
   * @param tokList list of string regexes
   * @return [[TokenRegex]] comprised of the elements of l turned into capturing groups, and OR'd together
   */
  def apply(tokList: List[String]): TokenRegex = {
    invalidTokens(tokList) match {
      case Some(inv) => throw new RuntimeException("Invalid token strings provided: " + inv)
      case _ => new TokenRegex(("(" + tokList.tail.foldLeft(tokList.head)((l, r) => l + "|" + r) + "){1}").r)
    }
  }

  /**
   * @param tokList list of strings whose elements represent a regex for a token
   * @return [[None]] if all tokens are valid, otherwise [[Some]] containing a string of all invalid tokens
   */
  def invalidTokens(tokList: List[String]): Option[String] = {
    val l = tokList.filterNot(isValidTokenString)
    l match {
      case Nil => None
      case _ => Some(l.mkString(","))
    }
  }

  /**
   * @param s string to validate
   * @return true if the string represents a valid token, false otherwise
   */
  def isValidTokenString(s: String) = !Option(s).forall(_.isBlank)

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
