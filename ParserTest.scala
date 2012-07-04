package Tests

import scala.util.parsing.combinator._

/**
 * Created with IntelliJ IDEA.
 * User: hodapple
 * Date: 6/24/12
 * Time: 6:26 PM
 * To change this template use File | Settings | File Templates.
 */


object ParserTest extends CF3 {
    def main(args: Array[String]) {
        List(
            "1 + 2",
            "a + b",
            "(a + b) + c",
            "cos(54)",
            "cos(54) + (a*b)",
            "-1.0",
            "1.0",
            "1.0",
            "1.0 - 2.0",
            "1.0 / 2.0 + a",
            "a + cos(-4.0 + b)"
        ).foreach { str => {
            print("Parsing '" + str + "'...")
            parseAll(expr, str) match {
                case Success(exp: Expr, _) => println(exp)
                case Error(msg, _) => println("Error: " + msg)
                case Failure(msg, _) => println("Failure: " + msg)
                case _ => println("Error, unexpected result!")
            }
        }}
    }

}

// TODO:
// 1. Fix the expression parsing - it has some kinks.
// 2. Start adding in examples from the Context Free wiki to the ParserTest
// object. See if they parse!