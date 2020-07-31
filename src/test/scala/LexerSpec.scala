import com.dakers.compiler.Token
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class LexerSpec extends AnyFlatSpec {

"A Token's string representation" should "wrap the token as a string in quotation marks" in {
    val tokStr = "abc"
    Token(tokStr).toString should be ("\"" + tokStr + "\"")
  }

}
