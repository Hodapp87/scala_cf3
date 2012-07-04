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
            "startshape FOO[]"
        ).foreach { str => printParseMsg(str, parseAll(directive, str)) }
    }

    def printParseMsg(str: String, result: ParseResult[Any]) {
        print("Parsing '" + str + "': ")
        result match {
            case Success(exp: Any, _) => println(exp)
            case Error(msg, _) => println("ERROR: " + msg)
            case Failure(msg, _) => println("FAILURE: " + msg)
            case _ => println("Error, unexpected result!")
        }
    }

}

// TODO:
// 1. Fix the expression parsing - it has some kinks.
// 2. Start adding in examples from the Context Free wiki to the ParserTest
// object. See if they parse!