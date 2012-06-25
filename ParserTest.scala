package Tests

/**
 * Created with IntelliJ IDEA.
 * User: hodapple
 * Date: 6/24/12
 * Time: 6:26 PM
 * To change this template use File | Settings | File Templates.
 */


object ParserTest extends CF3 {
    def main(args: Array[String]) {
        List("a + b"
            ).foreach { x => println(parseAll(expr, x))}
    }
}

// TODO:
// 1. Fix the expression parsing - it is broken yet again. See the example in
// the Master
// 2. Start adding in examples from the Context Free wiki to the ParserTest
// object. See if they parse!