package Tests

import scala.util.parsing.combinator._

/* Still needed:
 - 'tile' directive
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
    case class Import(filename: String, namespace : String) extends Directive
    case class Startshape(shape: String, adj: Seq[Adjust]) extends Directive
    case class FuncDef(name: String, body: Expr, argDecl: Seq[VarDecl]) extends Directive
    case class ShapeDeclaration(name: String, argDecl: Seq[VarDecl], rules: Seq[ShapeRule]) extends Directive
    case class PathDeclaration(name: String, argDecl: Seq[VarDecl], rules: Seq[PathElem]) extends Directive
    // We might need to include scope in this one (maybe outside of the AST)
    case class VarDecl(name: String, varType: String, value: Expr) extends Directive

    // Control flow primitives
    // (Go anywhere a shape replacement, path operation or path command can go)
    sealed abstract class Control extends Directive
    case class Loop() extends Control
    case class IfBranch() extends Control
    case class Switch() extends Control
    case class Transform() extends Control
    case class Clone() extends Control // is this control flow?

    // Configuration variables (CF:: namespace)?

    // Expressions
    sealed abstract class Expr
    case class Const(constVal: Float) extends Expr
    case class ArithOperation(op: String, args: Seq[Expr]) extends Expr
    case class FuncCall(fname: String, args: Seq[Expr]) extends Expr
    case class Variable(name: String) extends Expr

    // I really should replace 'Shape' with something else...
    sealed abstract class Shape
    case class ShapeRule(weight: Float, shapes: Seq[ShapeElem]) extends Shape
    sealed abstract class ShapeElem
    case class ShapeControl(ctl: Control, contents: Seq[ShapeElem]) extends ShapeElem
    case class ShapePrimitive(p: Prim, adj: Seq[Adjust]) extends ShapeElem
    case class ShapeReplacement(name: String, adj: Seq[Adjust], params: Seq[VarDecl]) extends ShapeElem

    // Includes both shape and color adjustment:
    case class Adjust(op: String, args: Seq[Expr]) extends Shape // shouldn't extend Shape, really

    sealed abstract class PathElem
    // PathControl is if you have control flow moderating several path elements:
    case class PathControl(ctl: Control, contents: Seq[PathElem]) extends PathElem
    case class PathOperation(name: String, flags: String, args: Seq[Expr]) extends PathElem
    // Path commands can be stroke or fill:
    case class PathCommand(cmd: String, flags: String, adj: Seq[Adjust]) extends PathElem
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

