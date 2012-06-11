package Tests

import scala.util.parsing.combinator._

/* Still needed:
 - 'tile' directive
 - 'startshape' directive
 - 'include' directive
 - 'background' directive
 - Loops (both old and new way?)
   - e.g. integer * [ adjustments ] name [ adjustments ]
   - and integer * [ adjustments ] { replacements }
 - Both ordered (with []) and unordered (with {}) adjustments.
 - Color adjustments
 - All path operations
 */

// I also still need a top-level rule for the grammar.

object Prim extends Enumeration {
    type Prim = Value
    val Square, Triangle, Circle = Value
}
import Prim._

trait AST {
    sealed abstract class Directive
    case class Import(filename : String, namespace : String) extends Directive
    case class Startshape(shape : String) extends Directive // plus a shape adjustment
    case class FuncDef(name : String, body : Expr) extends Directive // plus an argument list
    case class ShapeRule(name : String) extends Directive // plus shape instantiations and/or control structures
    case class PathRule(name : String) extends Directive // plus path primitives and/or control structures
    case class VarDecl(name : String, value : Expr) extends Directive // we'll have to include scope somehow too
    case class Loop() extends Directive
    case class IfBranch() extends Directive
    case class Switch() extends Directive
    case class Transform() extends Directive
    case class Clone() extends Directive

    // Configuration variables (CF:: namespace)?
    sealed abstract class Expr

    sealed abstract class Shape
    case class ShapePrimitive(p : Prim) extends Shape // needs adjustment
    case class ShapeInstance(name : String) extends Shape // needs adjustment
}

abstract class Expr
case class Const(constVal : Float) extends Expr
case class ShapeDecl(name : String, repl : ShapeReplace) extends Expr
case class ShapeReplace(weight : Float, shapes: Seq[Expr]) extends Expr
case class ShapePrimitive(p: Prim, adjust: Seq[AdjustOperation]) extends Expr
case class ShapeInstance(name: String, adjust: Seq[AdjustOperation]) extends Expr
case class AdjustOperation(op: String, args: Seq[Expr]) extends Expr
case class ArithOperation(op: String, operand1: Expr, operand2: Expr) extends Expr
case class FuncCall(fname: String, args: Seq[Expr]) extends Expr
case class ArgDecl(typename: String, name: String) extends Expr
case class ArgList(args: Seq[ArgDecl]) extends Expr

class CF3 extends RegexParsers {
    // Directives I have yet to use fully
    def importFile = "import"
    def include = "include"
    //
    val IDENT = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
    val CONST = """[0-9]+(\.[0-9]+)?"""r
    def expr : Parser[Expr] =
        (factor~rep(binop~factor) ^^
            { case f~l => (f /: l)
                { case (acc, op ~ operand) => ArithOperation(op, acc, operand) } }
        |unop~factor ^^ { case op ~ f => ArithOperation(op, Const(0.0f), f)})
    def factor : Parser[Expr] =
        (CONST ^^ {x => Const(x toFloat)}
        |"(" ~> expr <~ ")"
        |IDENT ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case f~args => FuncCall(f, args)})
    def unop = ( "-" )
    // Yes, order matters here! Smaller items should go after larger ones if
    // they begin the same way (or something like that)
    def binop = ( "-" | "^^" | "+-" | "*" | "/" | "+" | "..." | ".." | "<="
        | ">=" | "<>" | ">" | "<" | "==" | "&&" | "^" )
    def shapedecl : Parser[Expr] =
        ("shape" ~> IDENT ~ opt("(" ~> argdecl <~ ")") ~ shapereplace) ^^
            { case id~arg~replace => ShapeDecl(id, replace) }
    def shapereplace : Parser[ShapeReplace] =
        (opt("rule" ~> opt(CONST)) ~ ("{" ~> rep(shape) <~ "}")) ^^
            { case None ~ s          => ShapeReplace(1.0f, s)
              case Some(None) ~ s    => ShapeReplace(1.0f, s)
              case Some(Some(w)) ~ s => ShapeReplace(w toFloat, s) } // FIXME: will fail with percents!
    def argdecl : Parser[ArgList] =
        (repsep(opt(argtype) ~ IDENT, ",") ^^
            { l => ArgList(l.map({ case None~id    => ArgDecl("number", id)
                                   case Some(t)~id => ArgDecl(t, id) } ))})
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    // TODO: Make this actually use adjustments:
    // TODO: Make this use arguments too!
    def shape : Parser[Expr] =
        ( primitive ~ opt(adjust) ^^
            { case prim~None => prim
              case prim~Some(adj) => ShapePrimitive(prim.p, adj) }
        | IDENT ~ opt(("(" ~> opt(arglist) <~ ")")) ~ adjust ^^ { case id~args~adj => ShapeInstance(id, adj) } )
    def arglist = repsep(expr, ",")
    // TODO: Check for other primitives?
    def primitive : Parser[ShapePrimitive] =
        ( "SQUARE"   ^^ { _ => ShapePrimitive(Prim.Square, List()) }
        | "TRIANGLE" ^^ { _ => ShapePrimitive(Prim.Triangle, List()) }
        | "CIRCLE"   ^^ { _ => ShapePrimitive(Prim.Circle, List()) }  )
    def adjust = "[" ~> rep(operation) <~ "]"
    def operation : Parser[AdjustOperation] =
        /*( "x" ~ expr | "x" ~ expr ~ expr
        | "x" ~ expr ~ expr ~ expr | "y" ~ expr | "z" ~ expr
        | "size" ~ expr | "s" ~ expr | "size" ~ expr ~ expr
        | "s" ~ expr ~ expr | "rotate" ~ expr | "r" ~ expr
        | "flip" ~ expr | "f" ~ expr | "skew" ~ expr ~ expr ) ^^*/
        (IDENT ~ rep1(expr) ^^
            { case op~args => AdjustOperation(op, args) })
        // This is a coarse way to do things. I should change IDENT to something
        // else, and I must validate 'expr' further.
        // I should consider organizing based on number of arguments.
        // Also, I need the color adjustents too.
}

object ParseExpr extends CF3 {
    /*def main(args: Array[String]) {
        println("input : "+ args(0))
        println(parseAll(shapedecl, args(0)))
    }*/
    def parseStr(s: String) = parseAll(shapedecl, s)
    def parseStrExpr(s: String) = parseAll(expr, s)
}

