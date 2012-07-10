import Tests.AST;
import Tests.CF3;
import Tests.ParserTest;

// Put the JAR built from the Scala code in the 'code' directory of this
// sketch.

void setup() {
    String line = "shape TestShape rule 0.1% { }";
    System.out.println("Starting...");
    CF3 parser = new CF3();
    System.out.println("Instantiated CF3!");
    parser.parseAll(parser.directive(), line);
    System.out.println("Parsed successfully!");

    // Now do something else completely to bypass the generics in CF3...
    AST.Directive dir = ParserTest.getDirective(line);
    String name;
    if (dir instanceof AST.Import) {
        name = "Import";
    } else if (dir instanceof AST.Startshape) {
        AST.Startshape shape = (AST.Startshape) dir;
        name = "Startshape, name=";
        name += shape.shape();
    } else if (dir instanceof AST.FuncDef) {
        name = "FuncDef";
    } else if (dir instanceof AST.ShapeDeclaration) {
        AST.ShapeDeclaration shape = (AST.ShapeDeclaration) dir;
        name = "ShapeDeclaration, name=";
        name += shape.name();
    } else if (dir instanceof AST.PathDeclaration) {
        name = "PathDeclaration";
    } else if (dir instanceof AST.VarDecl) {
        name = "VarDecl";
    } else {
        name = "Unknown directive type!";
    }

    System.out.println(name);
}

void draw() {
}

