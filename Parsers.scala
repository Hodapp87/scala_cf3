package Tests

import scala.util.parsing.combinator._

/* Still needed:
 - Loops (both old and new way?)
   - e.g. integer * [ adjustments ] name [ adjustments ]
   - and integer * [ adjustments ] { replacements }
 - Both ordered (with []) and unordered (with {}) adjustments.
 - Color adjustments
 - All path operations
 - Anything in CF:: namespace
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
    sealed abstract class Control
    case class Loop() extends Control
    case class IfBranch() extends Control
    case class Switch() extends Control
    case class Transform() extends Control
    case class Clone() extends Control // is this control flow?

    // Configuration variables (CF:: namespace)?

    // Expressions
    sealed abstract class Expr
    case class Const(constVal: Float) extends Expr
    case class ArithOperation(op: String, opnd1: Expr, opnd2: Expr) extends Expr
    case class FuncCall(fname: String, args: Seq[Expr]) extends Expr
    case class Variable(name: String) extends Expr

    // I really should replace 'Shape' with something else...
    sealed abstract class Shape
    case class ShapeRule(weight: Float, shapes: Seq[ShapeElem]) extends Shape
    sealed abstract class ShapeElem
    case class ShapeControl(ctl: Control, contents: Seq[ShapeElem]) extends ShapeElem
    case class ShapePrimitive(p: Prim, adj: Seq[Adjust]) extends ShapeElem
    case class ShapeReplacement(name: String, adj: Seq[Adjust], params: Seq[Expr]) extends ShapeElem

    // Includes both shape and color adjustment:
    sealed abstract class Adjustment
    case class Adjust(op: String, args: Seq[Expr]) extends Adjustment

    sealed abstract class PathElem
    // PathControl is if you have control flow moderating several path elements:
    case class PathControl(ctl: Control, contents: Seq[PathElem]) extends PathElem
    case class PathOperation(name: String, flags: String, args: Seq[Expr]) extends PathElem
    // Path commands can be stroke or fill:
    case class PathCommand(cmd: String, flags: String, adj: Seq[Adjust]) extends PathElem
}

class CF3 extends RegexParsers with AST {
    // Start rule
    def directive = include | startshape | shapedecl
    def include = ("import" | "include") ~> FILENAME ^^ { Import(_, "need namespace") }

    val FILENAME = """(\S+)|(\".+\")"""r
    val IDENT = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
    val CONST = """[0-9]+(\.[0-9]+)?"""r
    def expr : Parser[Expr] =
        (unop~expr ^^ { case op ~ exp => ArithOperation(op, Const(0.0f), exp)}
        |factor~rep(binop~factor) ^^
            { case f~l => (f /: l)
                { case (acc, op ~ operand) => ArithOperation(op, acc, operand) } })
    def factor : Parser[Expr] =
        (CONST ^^ {x => Const(x toFloat)}
        |"(" ~> expr <~ ")"   // 'expr' is already Parser[Expr]
        |IDENT ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case f~args => FuncCall(f, args)}
        |IDENT ^^ {x => Variable(x)})
        // N.B. variable reference must proceed function call here or a
        // function call will improperly parse as a variable
    def unop = ( "-" )
    // Yes, order matters here! Smaller items should go after larger ones if
    // they begin the same way (or something like that)
    def binop = ( "-" | "^^" | "+-" | "*" | "/" | "+" | "..." | ".." | "<="
        | ">=" | "<>" | ">" | "<" | "==" | "&&" | "^" )
    // We arbitrarily decide that 'startshape' cannot have parameters and
    // cannot be a primitive (the Wiki doesn't specify)
    def startshape = ("startshape" ~> IDENT ~ opt(adjust)) ^^
        { case id~None => Startshape(id, List())
          case id~Some(adjust) => Startshape(id, adjust) }
    def shapedecl : Parser[Directive] =
        ("shape" ~> IDENT ~ opt("(" ~> argdecl <~ ")") ~ shapereplace) ^^
            { case id~None~replace => ShapeDeclaration(id, List(), List(replace)) // fix this!
              case id~Some(arg)~replace => ShapeDeclaration(id, arg, List(replace))} // fix this!
    // FIXME: This rule will fail with percents, but they are valid weights.
    def shapereplace : Parser[ShapeRule] =
        (opt("rule" ~> opt(CONST)) ~ ("{" ~> rep(shape) <~ "}")) ^^
            { case None ~ s          => ShapeRule(1.0f, s)
              case Some(None) ~ s    => ShapeRule(1.0f, s)
              case Some(Some(w)) ~ s => ShapeRule(w toFloat, s) }
    def argdecl = repsep(vardecl, ",")
    def vardecl = (opt(argtype) ~ IDENT) ^^
        { case None ~ id  => VarDecl(id, "number", Const(0.0f))
          case Some(t)~id => VarDecl(id, t, Const(0.0f)) }
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    // TODO: Make this actually use adjustments:
    // TODO: Make this use arguments too!
    def shape : Parser[ShapeElem] =
        ( primitive ~ opt(adjust) ^^
            { case prim~None => prim
              case prim~Some(adj) => ShapePrimitive(prim.p, adj) }
        | IDENT ~ opt(("(" ~> opt(arglist) <~ ")")) ~ adjust ^^
            { case id~None~adj => ShapeReplacement(id, adj, List())
              case id~Some(None)~adj => ShapeReplacement(id, adj, List())
              case id~Some(Some(arg))~adj => ShapeReplacement(id, adj, arg)
            } )
    def arglist = repsep(expr, ",")
    // TODO: Check for other primitives?
    def primitive : Parser[ShapePrimitive] =
        ( "SQUARE"   ^^ { _ => ShapePrimitive(Prim.Square, List()) }
        | "TRIANGLE" ^^ { _ => ShapePrimitive(Prim.Triangle, List()) }
        | "CIRCLE"   ^^ { _ => ShapePrimitive(Prim.Circle, List()) }  )
    def adjust = "[" ~> rep(operation) <~ "]"
    def operation : Parser[Adjust] =
        ( ((("size" | "s") ~ repN(3, expr))
        | (("size" | "s" | "skew") ~ repN(2, expr))
        | (("x"|"y"|"z"|"rotate"|"r"|"flip"|"f"|"size"|"s") ~ repN(1, expr)))
            ^^ { case op~args => Adjust(op, args)} )
        // I need the color adjustments too.
}
