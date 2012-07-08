package Tests

import scala.util.parsing.combinator._

object ParserTest extends CF3 {
    def main(args: Array[String]) {
        println("Trying expressions: ")
        List(
            "1 + 2",
            "a + b",
            "(a + b) + c",
            "cos(54)",
            "cos(54) + (a*b)",
            "someLongerFunction(arg1, arg2, arg3, 1.0 - 2.0)",
            "-1.0",
            "1.0",
            "1.0",
            "1.0 - 2.0",
            "-2.0 + 1.0",
            "1.0 / 2.0 + a",
            "a + cos((-4.0) + b)",
            "a + cos(-4.0 + b)"
        ).foreach { str => printParseMsg(str, parseAll(expr, str)) }

        println("Trying directives: ")
        List(
            "startshape FOO",
            "startshape FOO[ s 1 ]",
            "startshape FOO[ s 1 2 ]",
            "startshape FOO[ size 1 2 ]",
            "startshape FOO[ s 1 2 3 ]",
            "startshape FOO[ size 1 2 3 ]",
            "startshape FOO[ size 1 2 3 x (1+2)]",
            "startshape FOO[ size 1 2 3 x 1+2]",
            "startshape FOO[]",
            "shape TestShape { }",
            "shape TestShape { SQUARE [] }",
            "shape TestShape rule { SQUARE [] }",
            "shape TestShape rule 0.01 { SQUARE [] }",
            "shape TestShape rule 2% { SQUARE [] }",
            "shape TestShape rule 0.1% { }",
            //"rule BLAH { }", // should fail
            "shape curve(number shrink, number turn) { SQUARE [] curve(=) [y 0.5 r turn s shrink y 0.5] }"
        ).foreach { str => printParseMsg(str, parseAll(directive, str)) }
    }

    def printParseMsg(str: String, result: ParseResult[Any]) {
        print("Parsing '" + str + "': ")
        result match {
            case Success(exp: Any, _) => println(exp)
            case Error(msg, _) => println("ERROR: " + msg); throw new Exception(msg)
            case Failure(msg, _) => println("FAILURE: " + msg); throw new Exception(msg)
            case _ => throw new Exception("Unexpected type in ParseResult!")
        }
    }

}

// TODO:
// 2. Keep adding in examples from the Context Free wiki to the ParserTest
// object. See if they parse!
// 3. Is this valid yet?
/*
shape aShape
rule 50% {  // probability is 0.5
    SQUARE []
}
rule {      // probability is 1 * (1 - 0.5) / (1 + 2), or 1/6
    CIRCLE []
}
rule 2 {    // probability is 2 * (1 - 0.5) / (1 + 2), or 1/3
    TRIANGLE []
}
*/
// 4. Test this damn code as a JAR in Processing!