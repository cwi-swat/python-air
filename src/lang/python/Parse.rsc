@license{
Copyright (c) 2021, NWO-I Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
  
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
  
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
  
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
}
@synopsis{Uses the Python AST library to parse Python code and then maps it to a Rascal data-type}
@description{
    This module provides functions to produce abstract syntax trees for Python code.
    It supports expressions, statements and modules as top-level nodes.

    The functionality provided here depends heavily on the shape of the ASTs provided
    by the Python AST library. This AST format is rather abstract and does *not*
    retain the order of appearance of syntactical constructs in a file. It does
    provide start and end line/column information for all expressions and statements.

    This module is focused on enabling the most basic Python *analysis* in Rascal. It does not
    feature Python transformation features.
}
@author{Jurgen J. Vinju}
module lang::python::Parse

extend lang::python::AST;
import util::ShellExec;
import util::SystemAPI;
import lang::json::IO;
import IO;
import Type;
import ValueIO;

@synopsis="Installs the ast2json Python library using pip3"
public void installRequirements() {
    println(exec("pip3", args=["install", "ast2json"]));
}

@synopsis="Retrieves the search path for python files using the sys.path constant"
public list[loc] pythonPath() {
    lst=exec("python3", args=["-c", "import sys;print(sys.path)"]);
    println("lst: <lst>");
    lst=visit (lst) {
        case /\'/ => "\""
    }
    println("lst2: <lst>");
    return [ |file:///| + e | e <- readTextValueString(#list[str], lst)];
}

@synopsis="parses a python expression producing an AST of type Expression"
public Expression parsePythonExpression(str input, loc src) 
    = convertExp(importAST(input), src);

@synopsis="parses a python statement producing an AST of type Expression"
public Statement parsePythonStatement(str input, loc src) 
    = convertStat(importAST(input), src);

@synopsis="parses a python module producing an AST of type Module"
public Module parsePythonModule(str input, loc src) 
    = convertModule(importAST(input), src);

@synopsis="parses a python module producing an AST of type Module"
public Module parsePythonModule(loc src) 
    = convertModule(importAST(readFile(src)), src);

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

// modules

Module convertModule("object"(_type="Module", body=list[node] body, type_ignores=list[node] type_ignores), loc src) 
    = \module([convertStat(s, src) | s <- body], [convertTypeIgnore(i) | i <- type_ignores]);

Module convertModule("object"(_type="Expression", expr=node body), loc src) 
    = \expression(convertExp(body, src));    

Module convertModule("object"(_type="Interactive", body=list[node] body), loc src) 
    = \interactive([convertStat(s, src) | s <- body]);    

Module convertModule("object"(_type="FunctionType", argtypes=list[node] argtypes, expr=node returns), loc src) 
    = \functionType([convertExp(e, src) | e <- argtypes], convertExp(returns, src));

// statements

Statement convertStat(node obj:"object"(_type=str typ), loc src) 
    = convertStat(typ, obj, src)
        [src=obj has lineno 
            ? src(0,1,<\int(obj.lineno), \int(obj.col_offset)>,<\int(obj.end_lineno), \int(obj.end_col_offset)>) 
            : src];

Statement convertStat("Expression", node obj, loc src)
    = expr(convertExp(obj, src));

Statement convertStat("FunctionDef",
    node obj:"object"(
        name=str name,
        args=node formals,
        body=list[node] body,
        decorators=list[node] decorators
    ),
    loc src)
    = functionDef(
        id(name), 
        convertArgs(formals, src), 
        [convertStat(s, src) | s <- body], 
        [convertExp(e, src) | e <- decorators], 
        obj.returns? ? just(convertExp(obj.returns, src)) : nothing(),
        obj.typeComment? ? just(obj.typeComment) : nothing()
    );

Statement convertStat("AsyncFunctionDef",
    node obj:"object"(
        name=str name,
        args=node formals,
        body=list[node] body,
        decorators=list[node] decorators
    ),
    loc src)
    = asyncFunctionDef(
        id(name), 
        convertArgs(formals, src), 
        [convertStat(s, src) | s <- body], 
        [convertExp(e, src) | e <- decorators], 
        obj.returns? ? just(convertExp(obj.returns, src)) : nothing(),
        obj.typeComment? ? just(obj.typeComment) : nothing()
    );

Statement convertStat("ClassDef",
    node obj:"object"(
        name=str name,
        based=list[node] bases,
        keywords=list[node] keywords,
        body=list[node] body,
        decorators=list[node] decorators
    ),
    loc src)
    = classDef(
        id(name),
        [convertExp(b, src) | b <- bases],
        [convertKeyword(k, src) | k <- keywords],
        [convertStat(s, src) | s <- body],
        [convertExp(d, src) | d <- decorators]
    );

Statement convertStat("Return", node obj, loc src) 
    = \return(obj.\value? ? just(convertExp(obj.\value, src)) : nothing());

Statement convertStat("Delete", "object"(target=list[node] targets), loc src)
    = delete([convertExp(t, src) | t <- targets]);

Statement convertStat("Assign", node obj:"object"(target=list[node] targets, \value=node \val), loc src)
    = assign(
        [convertExp(t, src) | t <- targets],
        convertExp(val, src),
        obj.type_comment? ? just(obj.type_comment) : nothing()
    );

Statement convertStat("AnnAssign", node obj:"object"(target=node target, annotation=node annotation), loc src)
    = annAssign(
        convertExp(target, src), 
        convertExp(annotation, src),
        obj.\value? ? just(convertExp(obj.\value, src)) : nothing(),
        obj.simple == 1
    );

Statement convertStat("AugAssign", "object"(op=node op, target=node target, \value=node v), loc src)
    = convertAssign(op._type, convertExp(target, src), convertExp(v, src));


Statement convertStat("For", 
    node obj:"object"(
        target=node target,
        iter=node iter,
        body=list[node] body,
        or_else=list[node] or_else
    ),
    loc src
    )
    = \for(
        convertExp(target, src),
        convertExp(iter, src),
        [convertStat(s, src) | s <- body],
        [convertStat(s, src) | s <- or_else],
        obj.type_comment? ? just(obj.type_comment) : nothing()
    );

Statement convertStat("AsyncFor", 
    node obj:"object"(
        target=node target,
        iter=node iter,
        body=list[node] body,
        or_else=list[node] or_else
    ),
    loc src
    )
    = \asyncFor(
        convertExp(target, src),
        convertExp(iter, src),
        [convertStat(s, src) | s <- body],
        [convertStat(s, src) | s <- or_else],
        obj.type_comment? ? just(obj.type_comment) : nothing()
    );

Statement convertStat("While", 
    node obj:"object"(
        target=node \test,
        body=list[node] body,
        or_else=list[node] or_else
    ),
    loc src
    )
    = \while(
        convertExp(\test, src),
        [convertStat(s, src) | s <- body],
        [convertStat(s, src) | s <- or_else]
    );

Statement convertStat("If", 
    node obj:"object"(
        target=node \test,
        body=list[node] body,
        or_else=list[node] or_else
    ),
    loc src
    )
    = \if(
        convertExp(\test, src),
        [convertStat(s, src) | s <- body],
        [convertStat(s, src) | s <- or_else]
    );

Statement convertStat("With", 
    node obj:"object"(
        items=list[node] items,
        body=list[node] body
    ),
    loc src
    )
    = \with(
        [convertItem(i, src) |  i <- items],
        [convertStat(s, src) | s <- body],
        obj.type_comment? ? just(obj.type_comment) : nothing()
    );

Statement convertStat("AsyncWith", 
    node obj:"object"(
        items=list[node] items,
        body=list[node] body
    ),
    loc src
    )
    = \asyncWith(
        [convertItem(i, src) |  i <- items],
        [convertStat(s, src) | s <- body],
        obj.type_comment? ? just(obj.type_comment) : nothing()
    );    

Statement convertStat("Raise", node obj, loc src)
    = raise(
        obj.exc? ? just(convertExp(obj.exc, src)) : nothing(),
        obj.cause? ? just(convertExp(obj.cause, src)) : nothing()
    ); 

Statement convertStat("Try", 
    "object"(
        body=list[node] body,
        handlers=list[node] handlers,
        or_else=list[node] or_else,
        final_body=list[node] final_body
    ),
    loc src)
    = \try(
        [convertStat(s, src) | s <- body],
        [convertHandler(h, src) | h <- handlers],
        [convertStat(s, src) | s <- or_else],
        [convertStat(s, src) | s <- final_body]
    );

Statement convertStat("Assert", node obj:"object"(\test=node t), loc src)
    = \assert(convertExp(t, src), obj.msg? ? just(convertExp(obj.msg, src)) : nothing());

Statement convertStat("Import", "object"(aliases=list[node] aliases), loc src)
    = \import([convertAlias(a) | a <- aliases]);

Statement convertStat("ImportFrom", node obj:"object"(aliases=list[node] aliases), loc src)
    = \importFrom(
        obj.\module? ? just(id(obj.\module)) : nothing(), 
        [convertAlias(a) | a <- aliases],
        obj.level? ? just(obj.level) : nothing()
    );

Statement convertStat("Global", "object"(names=list[str] names), loc src) 
    = global([id(i) | i <- names]);

Statement convertStat("NonLocal", "object"(names=list[str] names), loc src) 
    = nonlocal([id(i) | i <- names]);

Statement convertStat("Pass", _, loc src) = pass();
Statement convertStat("Break", _, loc src) = \break();
Statement convertStat("Continue", _, loc src) = \continue();

// expressions
Expression convertExp(node obj:"object"(_type=str typ), loc src) 
    = convertExp(typ, obj, src)
        [src=obj has lineno 
            ? src(0,1,<\int(obj.lineno), \int(obj.col_offset)>,<\int(obj.end_lineno), \int(obj.end_col_offset)>) 
            : src];

Expression convertExp("Expression", node obj, loc src) = convertExp(obj.body, src);

Expression convertExp("Attribute", "object"(\value=node v, attr=str a, ctx=node ctx), loc src)
    = attribute(convertExp(v, src), id(a), convertCtx(ctx));

Expression convertExp("Subscript", "object"(\value=node v, slice=node slice, ctx=node ctx), loc src)
    = subscript(convertExp(v, src), convertExp(slice, src), convertCtx(ctx));

Expression convertExp("Subscript", "object"(\value=node v, ctx=node ctx), loc src)
    = starred(convertExp(v, src), convertCtx(ctx));

Expression convertExp("Name", "object"(\id=str i, ctx=node ctx), loc src)
    = name(id(i), convertCtx(ctx));

Expression convertExp("Tuple", "object"(elts=list[node] elts, ctx=node ctx), loc src)
    = \tuple([convertExp(e, src) | e <- elts], convertCtx(ctx));

Expression convertExp("Lambda", "object"(args=node args, body=node body), loc src) 
    = lambda(convertArgs(args, src), convertExp(body, src));

Expression convertExp("Slice", node obj, loc src) 
    = \slice(
        obj.lower? ? just(convertExp(obj.lower, src)) : nothing(),
        obj.upper? ? just(convertExp(obj.upper, src)) : nothing(),
        obj.step? ? just(convertExp(obj.step, src)) : nothing()
    );

Expression convertExp("JoinedStr", "object"(values=list[node] values), loc src)
   = joinedStr([convertExp(e, src) | e <- values]);

Expression convertExp("BinOp", "object"(op=node op, \left=node lhs, \right=node rhs), loc src) 
    = convertOp(op._type, convertExp(lhs, src), convertExp(rhs, src));

Expression convertExp("Compare", "object"(left=node first, ops=list[node] ops, comparators=list[node] comps), loc src)
    = compare(convertExp(first, src), [convertCompOp(op) | "object"(_type=str op) <- ops], [convertExp(c, src) | c <- comps]);

Expression convertExp("UnaryOp", "object"(op=node op, operand=node arg), loc src) 
    = convertOp(op._type, convertExp(arg, src));    

Expression convertExp("Name", "object"(ctx=node c, id=str n), loc src) = name(id(n), convertCtx(c));

Expression convertExp("Call", "object"(func=node f, args=list[node] as, keywords=list[node] kws), loc src) 
    = call(
        convertExp(f, src), 
        [convertExp(a, src) | a <- as], 
        [convertKeyword(kw, src) | kw <- kws] 
    );

Expression convertExp("FormattedValue", node obj:"object"(\value=node v), loc src)
    = formattedValue(
        convertExp(v, src), 
        obj.conversion? ? just(convertConv(\int(obj.conversion))) : nothing(), 
        obj.formatSpec? ? just(convertExp(obj.formatSpec, src)) : nothing()
    );


Expression convertExp("List", "object"(elts=list[node] elts, ctx=node ctx), loc src) 
    = \list([convertExp(e, src) | e <- elts], convertCtx(ctx));

Expression convertExp("Set", "object"(elts=list[node] elts), loc src) 
    = \set([convertExp(e, src) | e <- elts]);

Expression convertExp("Dict",  "object"(keys=list[node] keys, values=list[node] values), loc src) 
    = \dict([convertExp(e, src) | e <- keys], [convertExp(e, src) | e <- values]);    

Expression convertExp("BoolOp", "object"(op="object"(_type="And"), values=list[node] vs), loc src)
    = and([convertExp(v, src) | v <- vs]);

Expression convertExp("BoolOp", "object"(op="object"(_type="Or"), values=list[node] vs), loc src)
    = and([convertExp(v, src) | v <- vs]);    

Expression convertExp("IfExp", "object"(\test=node t, body=node b, orelse=node orelse), loc src)
    = ifExp(convertExp(t, src), convertExp(b, src), convertExp(orelse, src));

Expression convertExp("ListComp", "object"(elt=node e, generators=list[node] gens), loc src)
    = listComp(convertExp(e, src), [convertGenerator(g, src) | g <- gens]);

Expression convertExp("SetComp", "object"(elt=node e, generators=list[node] gens), loc src)
    = listComp(convertExp(e, src), [convertGenerator(g, src) | g <- gens]);

Expression convertExp("GeneratorExp", "object"(elt=node e, generators=list[node] gens), loc src)
    = generatorExp(convertExp(e, src), [convertGenerator(g, src) | g <- gens]);

Expression convertExp("DictComp", "object"(key=node k, \value=node v, generators=list[node] gens), loc src)
    = dictComp(convertExp(k, src), convertExp(v, src), [convertGenerator(g, src) | g <- gens]);

Expression convertExp("Await", "object"(expr=node e), loc src) 
    = await(convertExp(e, src));

Expression convertExp("Yield", node obj, loc src) 
    = yield(obj.\value? ? just(convertExp(obj.\value, src)) : nothing());    

Expression convertExp("YieldFrom", "object"(\value=node v), loc src) 
    = yieldFrom(convertExp(v, src));

Expression convertExp("Constant", "object"(\value=num v), loc src) = constant(number(v), nothing());

Expression convertExp("Constant", "object"(\value=str s), loc src) = constant(string(s), nothing());

// assorted helper constructs
WithItem convertItem(node obj:"object"(context_expr=node c), loc src)
    = withItem(convertExp(c, src), obj.optional_vars? ? just(convertExp(obj.optional_vars, src)) : nothing());

Alias convertAlias(node obj:"object"(name=str name))
    = \alias(id(name), obj.asname? ? just(id(obj.asname)) : nothing());

ExceptHandler convertHandler(
    node obj:"object"(
        body=list[node] body
    ),
    loc src)
    = exceptHandler(
        obj.\type? ? just(convertExp(obj.\type, src)) : nothing(),
        obj.name? ? just(id(obj.name)) : nothing(),
        [convertStat(s, src) | s <- body]
    )[src=obj has lineno 
            ? src(0,1,<\int(obj.lineno), \int(obj.col_offset)>,<\int(obj.end_lineno), \int(obj.end_col_offset)>) 
            : src];

CmpOp convertCompOp("Eq") = eq();
CmpOp convertCompOp("NotEq") = noteq();
CmpOp convertCompOp("Lt") = lt();
CmpOp convertCompOp("LtE") = lte();
CmpOp convertCompOp("Gt") = gt();
CmpOp convertCompOp("GtE") = gte();
CmpOp convertCompOp("Is") = is();
CmpOp convertCompOp("IsNot") = isnot();
CmpOp convertCompOp("In") = \in();
CmpOp convertCompOp("NotIn") = \notin();

Statement convertAssign("Sub", Expression target, Expression \value) = subAssign(target, \value);
Statement convertAssign("Mult", Expression target, Expression \value) = multAssign(target, \value);
Statement convertAssign("Matmult", Expression target, Expression \value) = matmultAssign(target, \value);
Statement convertAssign("Div", Expression target, Expression \value) = divAssign(target, \value);
Statement convertAssign("Mod", Expression target, Expression \value) = modAssign(target, \value);
Statement convertAssign("Pow", Expression target, Expression \value) = powAssign(target, \value);
Statement convertAssign("LShift", Expression target, Expression \value) = lshiftAssign(target, \value);
Statement convertAssign("RShift", Expression target, Expression \value) = rshiftAssign(target, \value);
Statement convertAssign("BitOr", Expression target, Expression \value) = bitorAssign(target, \value);
Statement convertAssign("BitXor", Expression target, Expression \value) = bitxorAssign(target, \value);
Statement convertAssign("BitAnd", Expression target, Expression \value) = bitandAssign(target, \value);
Statement convertAssign("FloorDiv", Expression target, Expression \value) = floordivAssign(target, \value);

Conversion convertConv(int i) = noFormatting() when i == -1;
Conversion convertConv(115) = stringFormatting();
Conversion convertConv(114) = reprFormatting();
Conversion convertConv(97) = asciiFormatting();

Keyword convertKeyword("object"(arg=str i, \value=node v), loc src) = \keyword(id(i), convertExp(v, src));

Comprehension convertGenerator("object"(target=node target, iter=node iter, ifs=list[node] ifs, isAsync=int isAsync), loc src) 
    = comprehension(
        convertExp(target, src),
        convertExp(iter, src),
        [convertExp(i, src) | i <- ifs],
        isAsync == 1
    );

Expression convertOp("Add", Expression l, Expression r) = add(l, r);
Expression convertOp("Sub", Expression l, Expression r) = sub(l, r);
Expression convertOp("Mult", Expression l, Expression r) = mult(l, r);
Expression convertOp("MatMult", Expression l, Expression r) = matmult(l, r);
Expression convertOp("Mod", Expression l, Expression r) = \mod(l, r);
Expression convertOp("Pow", Expression l, Expression r) = pow(l, r);
Expression convertOp("Div", Expression l, Expression r) = \div(l, r);
Expression convertOp("LShift", Expression l, Expression r) = lshift(l, r);
Expression convertOp("RShift", Expression l, Expression r) = rshift(l, r);
Expression convertOp("BitOr", Expression l, Expression r) = bitor(l, r);
Expression convertOp("BitXor", Expression l, Expression r) = bitxor(l, r);
Expression convertOp("BitAnd", Expression l, Expression r) = bitand(l, r);
Expression convertOp("FloorDiv", Expression l, Expression r) = floordiv(l, r);

Expression convertOp("Invert", Expression a) = invert(a);
Expression convertOp("Not", Expression a) = \not(a);
Expression convertOp("UAdd", Expression a) = uadd(a);
Expression convertOp("USub", Expression a) = usub(a);

ExprContext convertCtx("object"(_type="Load")) = load();

Arguments convertArgs(
    node obj:"object"(
        _type="arguments", 
        posonlyargs=list[node] posonlyargs,
        kwonlyargs=list[node] kwonlyargs,
        args=list[node] args
    ),
    loc src)
    = arguments(
        [convertArg(a, src) | a <- posonlyargs], 
        [convertArg(a, src) | a <- args], 
        obj.vararg? ? just(convertArg(obj.vararg, src)) : nothing(), 
        [convertArg(a, src) | a <- kwonlyargs], 
        obj.kw_defaults? ? [convertExp(e, src) | e <- nodes(obj.kw_defaults)] : [],
        obj.kwarg? ? just(convertArg(obj.kwarg, src)) : nothing(),
        obj.defaults? ? [convertExp(e, src) | e <- nodes(obj.defaults)] : []
    );

Arg convertArg(
    node obj:"object"(
        arg=str a
    ),
    loc src)
    = arg(
        id(a), 
        obj.annotation? ? just(convertExp(obj.annotation, src)) : nothing(),
        obj.type_comment ? just(obj.typeComment) : nothing() 
    )[src=obj has lineno 
            ? src(0,1,<\int(obj.lineno), \int(obj.col_offset)>,<\int(obj.end_lineno), \int(obj.end_col_offset)>) 
            : src];

TypeIgnore convertTypeIgnore("object"(_type="TypeIgnore", lineno=int l, \tag=str t))
    = typeIgnore(l, t);

// utilities

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

private int \int(value v) = typeCast(#int, v);
private list[node] nodes(value v) = typeCast(#list[node], v);