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
    case class Variable(name: String) extends Expr
    case class FuncCall(fname: String, args: Seq[Expr], captureOuter: Boolean) extends Expr
    // captureOuter means: call the function, but apply the arguments that the
    // calling function had (this assumes that the signatures match). If it's
    // true, 'args' is ignored.
    // The curve(=) call in this example means this:
    /* shape curve(number shrink, number turn) {
          SQUARE []
          curve(=) [[y 0.5 r turn s shrink y 0.5]]
       }
    */

    // I really should replace 'Shape' with something else...
    sealed abstract class Shape
    case class ShapeRule(weight: Float, shapes: Seq[ShapeElem]) extends Shape
    sealed abstract class ShapeElem
    case class ShapeControl(ctl: Control, contents: Seq[ShapeElem]) extends ShapeElem
    case class ShapePrimitive(p: Prim, adj: Seq[Adjust]) extends ShapeElem
    case class ShapeReplacement(name: String,
                                adj: Seq[Adjust],
                                params: Seq[Expr],
                                captureOuter: Boolean) extends ShapeElem
    // captureOuter here means the same thing as in FuncCall

    // Includes both shape and color adjustment:
    sealed abstract class Adjustment
    case class Adjust(op: String, args: Seq[Expr], reorder: Boolean) extends Adjustment
    // if 'reorder' is true, then this adjustment if it is within a group is
    // reordered to x, y, rotate, size, skew, & flip, dropping duplicates.

    sealed abstract class PathElem
    // PathControl is if you have control flow moderating several path elements:
    case class PathControl(ctl: Control, contents: Seq[PathElem]) extends PathElem
    //case class PathOperation(name: String, flags: String, args: Seq[Expr]) extends PathElem
    // Path commands can be stroke or fill:
    case class PathCommand(cmd: String, flags: String, adj: Seq[Adjust]) extends PathElem

    sealed abstract class PathOperation extends PathElem
    case class PathMove(x: Expr, y: Expr, rel: Boolean) extends PathElem
    case class PathLine(x: Expr, y: Expr, rel: Boolean) extends PathElem
    case class PathArc(x: Expr, y: Expr, rx: Expr, ry: Expr, ang: Expr, params: Seq[String], rel: Boolean) extends PathElem
    case class PathCurve(x: Expr, y: Expr, x1: Expr, y1: Expr, x2: Expr, y2: Expr) extends PathElem
    case class PathClosePoly(align: Boolean) extends PathElem
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
    // FIXME: There is no proper order of operations here!
    def factor : Parser[Expr] =
        (CONST ^^ {x => Const(x toFloat)}
        |"(" ~> expr <~ ")"   // 'expr' is already Parser[Expr]
        |IDENT ~ ("(" ~> "=" <~ ")") ^^ { case f~args => FuncCall(f, List(), true)}
        |IDENT ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case f~args => FuncCall(f, args, false)}
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
    def shapereplace : Parser[ShapeRule] =
        (opt("rule" ~> opt(ruleweight)) ~ ("{" ~> rep(shape) <~ "}")) ^^
            { case None ~ s          => ShapeRule(1.0f, s)
              case Some(None) ~ s    => ShapeRule(1.0f, s)
              case Some(Some(w)) ~ s => ShapeRule(w, s) }
    def ruleweight : Parser[Float] = (CONST~opt("%") ^^
        { case s~None => s toFloat
          case s~Some(_) => (s toFloat)/100.0f }) // optional percent sign is valid
    def argdecl = repsep(vardecl, ",")
    def vardecl = (opt(argtype) ~ IDENT) ^^
        { case None ~ id  => VarDecl(id, "number", Const(0.0f))
          case Some(t)~id => VarDecl(id, t, Const(0.0f)) }
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    // TODO: Make this actually use adjustments:
    // TODO: Make this use arguments too!
    def shape : Parser[ShapeElem] =
        ( shapePrim ~ opt(adjust) ^^
            { case prim~None => prim
              case prim~Some(adj) => ShapePrimitive(prim.p, adj) }
        // Instantiate shape with parameters captured from the calling scope:
        | IDENT ~ opt(("(" ~> "=" <~ ")")) ~ adjust ^^
            { case id ~ _ ~ adj => ShapeReplacement(id, adj, List(), true) }
        // Instantiate shape, optionally with explicit parameters
        | IDENT ~ opt(("(" ~> opt(arglist) <~ ")")) ~ adjust ^^
            { case id~None~adj => ShapeReplacement(id, adj, List(), false)
              case id~Some(None)~adj => ShapeReplacement(id, adj, List(), false)
              case id~Some(Some(arg))~adj => ShapeReplacement(id, adj, arg, false)
            } )
    // TODO: Check for other primitives?
    def shapePrim : Parser[ShapePrimitive] =
        ( "SQUARE"   ^^ { _ => ShapePrimitive(Prim.Square, List()) }
        | "TRIANGLE" ^^ { _ => ShapePrimitive(Prim.Triangle, List()) }
        | "CIRCLE"   ^^ { _ => ShapePrimitive(Prim.Circle, List()) }  )
    // this is clunky; we can't set the reorder tag without some context:
    def adjust = ( "{" ~> rep(operation) <~ "}" ^^
                        { _.map(adj => Adjust(adj.op, adj.args, true)) }
                 | "[" ~> rep(operation) <~ "]" )
    def arglist = repsep(expr, ",")
    def operation : Parser[Adjust] =
        ( ((("size" | "s") ~ repN(3, expr))
        | (("size" | "s" | "skew") ~ repN(2, expr))
        | (("x"|"y"|"z"|"rotate"|"r"|"flip"|"f"|"size"|"s") ~ repN(1, expr))
        | (("hue" | "h" | "saturation" | "sat") ~ repN(1, expr))
        | (("brightness" | "b" | "alpha" | "a") ~ repN(1, expr))
        | (("lhue" | "lh" | "lsaturation" | "lsat") ~ repN(1, expr))
        | (("lbrightness" | "lb" | "lalpha" | "la") ~ repN(1, expr)))
            ^^ { case op~args => Adjust(op, args, false)} )
    def pathMove =
        (operation("MOVETO", List("x", "y")) ^^ { case x~y => PathMove(x,y,false)}
        |operation("MOVEREL", List("x", "y")) ^^ { case x~y => PathMove(x,y,true)})
    def pathLine =
        (operation("LINETO", List("x", "y")) ^^ { case x~y => PathLine(x,y,false)}
        |operation("LINEREL", List("x", "y")) ^^ { case x~y => PathLine(x,y,true)})
    def pathOp(name: String, argsExp: Seq[String]) = {
        // TODO:
        // 1. Add an argument to this for a function like PathLine & PathMove
        // 2. Make this match on name+"TO", name+"REL" and use partial eval to
        // return a function with its last parameter set to true or false.
    }
    // 'operation' matches an expression like "name { arg1 value arg2 value }"
    // with arg1, arg2, etc. from 'argsExp'; it returns name~value~value...
    def operation(name: String, argsExp: Seq[String]) = {
        // Fails on empty lists. (Why is left as an exercise to the reader.)
        name ~> ("{" ~> (argsExp.map { _ ~> expr }.reduceLeft(_ ~ _)) <~ "}")
    }
}
