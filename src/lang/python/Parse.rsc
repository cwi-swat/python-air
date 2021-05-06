module lang::python::Parse

extend lang::python::AST;
import util::ShellExec;
import util::SystemAPI;
import lang::json::IO;
import IO;

@synopsis="Installs the ast2json Python library using pip3"
public void installRequirements() {
    println(exec("pip3", args=["install", "ast2json"]));
}

@synopsis="parses a python expression producing an AST of type Expression"
public Expression parsePythonExpression(str input, loc src) {
    json = importAST(input);
    return convertExp(json, src);
}

@synopsis="wraps the python ast library as an external system process"
@description{
    This function starts the python3 interpreter to have access
    to the ast.parse function from Python's standard library.
    The output of the parser is mapped to JSON and then imported
    into a rascal data-structure that can be converted later.

    This function should not be used by clients, but it can
    be called for debugging purposes since it reflects an 
    intermediate stage.
}
node importAST(str input) {
    tempDir = |file:///| + getSystemProperty("java.io.tmpdir");
    
    pythonParserFile = tempDir + "parsePython.py";
    pythonInputFile = tempDir + "pythonInputFile.py";

    if (!exists(pythonParserFile))
        writeFile(pythonParserFile, pythonParserCode());
    
    writeFile(pythonInputFile, input);

    output = exec("python3", args=[pythonParserFile.path, pythonInputFile.path]);
    
    return parseJSON(#node, output);
}

Expression convertExp(node obj:"object"(_type=str typ), loc src) 
    = convertExp(typ, obj, src)
        [src=obj has lineno 
            ? src(0,1,<\int(obj.lineno), \int(obj.col_offset)>,<\int(obj.end_lineno), \int(obj.end_col_offset)>) 
            : src];

Expression convertExp("Expression", node obj, loc src) = convertExp(obj.body, src);

Expression convertExp("BinOp", "object"(op="object"(_type="Add"), \left=node lhs, \right=node rhs), loc src) 
    = binOp(convertExp(lhs, src), add(), convertExp(rhs, src));

Expression convertExp("Constant", "object"(\value=num v), loc src) = constant(number(v), nothing());





private str pythonParserCode()
    = "import io
      'import os
      'import ast
      'import sys
      'from ast2json import ast2json
      'import json
      '
      'with open (sys.argv[1], \"r\") as aFile:
      '   data=aFile.read()
      '
      'theAst = ast.parse(data, mode=\"eval\")
      'theAstAsJson = ast2json(theAst)
      'theJsonAstAsString = json.dumps(theAstAsJson)
      '
      'print(theJsonAstAsString)
      ";

private int \int(value v) = cast(#int, v);

private &T cast(type[&T] t, value v) {
    if (&T r := v) {
        return r;
    }
    else {
        throw "could not parse <v> as <t>";
    }
}