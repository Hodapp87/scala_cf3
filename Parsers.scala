package Tests

import scala.util.parsing.combinator._
  
class Arith extends JavaTokenParsers {   
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

// What we have is not _just_ an expression we must simplify, however, this
// comes in at some point (as Context Free does have expressions we'll need to
// evaluate)

// I also still need a top-level production rule for the grammar.

object Prim extends Enumeration {
    type Prim = Value
    val Square, Triangle, Circle = Value
}
import Prim._

abstract class Expr
case class Const(constVal : Float) extends Expr
case class ShapeDecl(name : String, repl : ShapeReplace) extends Expr
case class ShapeReplace(weight : Float, shapes: Seq[Expr]) extends Expr
case class ShapePrimitive(p: Prim) extends Expr
case class ShapeInstance(name: String) extends Expr
case class AdjustOperation(op: String, args: Seq[Expr]) extends Expr
case class ShapeAdjust(ops: Seq[AdjustOperation]) extends Expr
case class ArithOperation(op: String, opnd1: Expr, opnd2: Expr) extends Expr
case class ArgDecl(typename: String, name: String) extends Expr
case class ArgList(args: Seq[ArgDecl]) extends Expr

class CF3 extends RegexParsers {
    val IDENT = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
    val CONST = """[0-9]+(\.[0-9]+)?"""r
    def expr : Parser[Expr] =
        ( CONST ^^ { v => Const(v toFloat) }
        | "(" ~> expr <~ ")"
        | expr ~ binop ~ expr ^^ { case e1~op~e2 => ArithOperation(op, e1, e2) }
        | unop ~ expr ^^ { case op~e1 => ArithOperation(op, Const(0.0f), e1) } )
        // Need this too:
        // | IDENT ~ "(" ~ expr ~ ")" ) ^^ { 
    def unop = ( "-" )
    def binop = ( "-" | "^" | "+" | "*" | "/" | "+-" | ".." | "..." | "<"
                | ">" | "<=" | ">=" | "==" | "<>" | "&&" | "^^" )
    // also, N.B. it need not be a pattern-matching function after
    def shapedecl: Parser[Expr] =
        ("shape" ~> IDENT ~ opt("(" ~> argdecl <~ ")") ~ shapereplace) ^^
            { case id~arg~replace => ShapeDecl(id, replace) }
    def shapereplace: Parser[ShapeReplace] =
        (opt("rule" ~> opt(CONST)) ~ ("{" ~> rep(shape) <~ "}")) ^^
            { case None ~ s          => ShapeReplace(1.0f, s)
              case Some(None) ~ s    => ShapeReplace(1.0f, s)
              case Some(Some(w)) ~ s => ShapeReplace(w toFloat, s) } // will fail with percents!
    // Note that weight can be a percent too
    def argdecl: Parser[ArgList] =
        (repsep(opt(argtype) ~ IDENT, ",") ^^
            { l => ArgList(l.map({ case None~id    => ArgDecl("number", id)
                                   case Some(t)~id => ArgDecl(t, id) } ))})
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    def shape: Parser[Expr] =
        ( primitive ~ opt(adjust) ^^ { case prim~adj => prim }
        | IDENT ~ ("(" ~> opt(arglist) <~ ")") ~ adjust  ^^ { case id~args~adj => ShapeInstance(id) } )
    def arglist = repsep(expr, ",")
    def primitive: Parser[Expr] =
        ( "SQUARE"   ^^ { _ => ShapePrimitive(Prim.Square) }
        | "TRIANGLE" ^^ { _ => ShapePrimitive(Prim.Triangle) }
        | "CIRCLE"   ^^ { _ => ShapePrimitive(Prim.Circle) }  )
    def adjust = "[" ~ rep(operation) ~ "]"
    def operation: Parser[Expr] =
        /*( "x" ~ expr | "x" ~ expr ~ expr
        | "x" ~ expr ~ expr ~ expr | "y" ~ expr | "z" ~ expr
        | "size" ~ expr | "s" ~ expr | "size" ~ expr ~ expr
        | "s" ~ expr ~ expr | "rotate" ~ expr | "r" ~ expr
        | "flip" ~ expr | "f" ~ expr | "skew" ~ expr ~ expr ) ^^*/
        (IDENT ~ rep(expr) ^^
            { case op~args => AdjustOperation(op, args) })
        // This is a coarse way to do things. I should change IDENT to something
        // else, and I must validate 'expr' further. 
    
}

class SimpleCF extends RegexParsers {
    val IDENT = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
    val CONSTANT = """[0-9]+(\.[0-9]+)?"""r
    def expr : Parser[Any] = ( CONSTANT | "(" ~ expr ~ ")"
               | expr ~ binop ~ expr | unop ~ expr | IDENT ~ "(" ~ expr ~ ")" )
    def unop = ( "-" )
    def binop = ( "-" | "^" | "+" | "*" | "/" | "+-" | ".." | "..." | "<"
                | ">" | "<=" | ">=" | "==" | "<>" | "&&" | "^^" )
    def shapedecl = ( "shape" ~ IDENT ~ shapereplace
                    | "shape" ~ IDENT ~ "(" ~ argdecl ~ ")" ~ shapereplace)
    def shapereplace = opt("rule" ~ opt(CONSTANT)) ~ "{" ~ rep(shape) ~ "}"
    // Note that weight can be a percent too
    def argdecl = repsep(opt(argtype) ~ IDENT, ",")
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    def shape = ( primitive ~ opt(adjust)
                | IDENT ~ opt(adjust)
                | IDENT ~ "(" ~ arglist ~ ")" ~ adjust )
    def arglist = repsep(expr, ",")
    def primitive = ( "SQUARE" | "TRIANGLE" | "CIRCLE" )
    def adjust = "[" ~ rep(operation) ~ "]"
    def operation = ( "x" ~ expr | "x" ~ expr ~ expr
                    | "x" ~ expr ~ expr ~ expr | "y" ~ expr | "z" ~ expr
                    | "size" ~ expr | "s" ~ expr | "size" ~ expr ~ expr
                    | "s" ~ expr ~ expr | "rotate" ~ expr | "r" ~ expr
                    | "flip" ~ expr | "f" ~ expr | "skew" ~ expr ~ expr )
    
}

class FullCF extends RegexParsers {
    val IDENT = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
    val CONSTANT = """[0-9]+(\.[0-9]+)?"""r
    def expr : Parser[Any] = ( CONSTANT | "(" ~ expr ~ ")"
               | expr ~ binop ~ expr | unop ~ expr | IDENT ~ "(" ~ expr ~ ")" )
    def unop = ( "-" )
    def binop = ( "-" | "^" | "+" | "*" | "/" | "+-" | ".." | "..." | "<"
                | ">" | "<=" | ">=" | "==" | "<>" | "&&" | "^^" )
    def shapedecl = ( "shape" ~ IDENT ~ shapereplace
                    | "shape" ~ IDENT ~ "(" ~ argdecl ~ ")" ~ shapereplace)
    def shapereplace = opt("rule" ~ opt(CONSTANT)) ~ "{" ~ rep(shape) ~ "}"
    // Note that weight can be a percent too
    def argdecl = repsep(opt(argtype) ~ IDENT, ",")
    def argtype = ("number" | "natural" | "adjustment" | "shape")
    def shape = ( primitive ~ adjust
                | IDENT ~ adjust
                | IDENT ~ "(" ~ arglist ~ ")" ~ adjust )
    def arglist = repsep(expr, ",")
    def primitive = ( "SQUARE" | "TRIANGLE" | "CIRCLE" )
    def adjust = "[" ~ rep(operation) ~ "]"
    def operation = ( "x" ~ expr | "x" ~ expr ~ expr
                    | "x" ~ expr ~ expr ~ expr | "y" ~ expr | "z" ~ expr
                    | "size" ~ expr | "s" ~ expr | "size" ~ expr ~ expr
                    | "s" ~ expr ~ expr | "rotate" ~ expr | "r" ~ expr
                    | "flip" ~ expr | "f" ~ expr | "skew" ~ expr ~ expr )
}

object ParseExpr extends CF3 {
    def main(args: Array[String]) {
        println("input : "+ args(0))
        println(parseAll(shapedecl, args(0)))
    }
    def parseStr(s: String) = parseAll(shapedecl, s)
}

