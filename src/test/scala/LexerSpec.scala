import com.dakers.compiler.{Token, TokenRegex, TokenRegexLexer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{a, be}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.matching.Regex

class LexerSpec extends AnyFlatSpec {

  "A Token's string representation" should "wrap the token as a string in quotation marks" in {
    val tokStr = "abc"
    Token(tokStr).toString should be("\"" + tokStr + "\"")
  }

  "The null pointer" should "not be a valid token string" in {
    TokenRegex.isValidTokenString(null) should be(false)
  }

  "The empty string" should "not be a valid token string" in {
    TokenRegex.isValidTokenString("") should be(false)
  }

  "A string containing only whitespace" should "not be a valid token string" in {
    TokenRegex.isValidTokenString(" ") should be(false)
  }

  "A valid regular expression" should "be a valid token string" in {
    TokenRegex.isValidTokenString("abc") should be(true)
  }

  "A list of all invalid tokens" should "return an Option containing a string comprised of the invalid elements joined by a delimiter" in {
    val e1 = ""
    val e2 = " "
    val l = List(e1, e2)
    TokenRegex.invalidTokens(l) shouldBe Some(l.mkString(","))
  }

  "A list of valid and invalid tokens" should "return an Option containing a string comprised of the invalid elements joined by a delimiter" in {
    val e1 = ""
    val e2 = " "
    val e3 = "abc"
    val e4 = "d+"
    TokenRegex.invalidTokens(List(e1, e2, e3, e4)) shouldBe Some(List(e1, e2).mkString(","))
  }

  "Constructing a token regex from an empty list of strings" should "throw a RuntimeException" in {
    a[RuntimeException] should be thrownBy TokenRegex(List())
  }

  "Constructing a token regex from a list containing exactly one invalid token" should "throw a RuntimeException" in {
    a[RuntimeException] should be thrownBy TokenRegex(List(""))
  }

  "Constructing a token regex from a list of more than 1 element, where all elements are invalid tokens" should "throw a RuntimeException" in {
    a[RuntimeException] should be thrownBy TokenRegex(List("", " "))
  }

  "Constructing a token regex from a list of more than 1 element, with at least one valid AND at least one invalid token" should "throw a RuntimeException" in {
    a[RuntimeException] should be thrownBy TokenRegex(List("abc", " "))
  }

  "A TokenRegex constructed from one regular expression" should "be constructed with a fixed quantifier and capturing group" in {
    val l = List("a")
    TokenRegex(l).re should be
    this.expectedRegEx(l)
  }

  "A TokenRegex constructed from multiple regular expressions" should "be constructed with a fixed quantifier and capturing group" in {
    val l = List("a", "b", "//d+")
    TokenRegex(l).re should be
    this.expectedRegEx(l)
  }

  def expectedRegEx(list: List[String]): Regex = {
    ("(" + list.tail.foldLeft(list.head)((l, r) => l + "|" + r) + "){1}").r
  }

  val basicTokens = List("\\{", "\\}", "\\(", "\\)", ";", "int", "return", "[a-zA-Z]\\w*", "[0-9]+")
  val basicLexer = new TokenRegexLexer(TokenRegex(basicTokens))

  val constPh = "CONST"
  val retConst = List("int", "main", "(", ")", "{", "return", constPh, ";", "}")

  def expectedOut(list: List[String] = retConst, i: Integer, ph: String = constPh): List[Token] = {
    list.map(x => if (x.equals(ph)) i.toString else x).map(Token)
  }

  def expectedIn(const: Integer) =
    s"""
       |int main(){
       |return $const;
       |}""".stripMargin

  def expectedMap(max: Integer) = Range(0, max).map(j => (expectedIn(j), expectedOut(i = j))).toMap

  "A program consisting of basic tokens" should "be properly tokenized" in {
    expectedMap(12).foreachEntry((in, out) => basicLexer.tokenize(in) should be(out))
  }

}
