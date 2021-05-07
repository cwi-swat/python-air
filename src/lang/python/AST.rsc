@license{
Copyright (c) 2021, NWO-I Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
  
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
  
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
  
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
}
@origin{This module was inspired by translating the code on https://docs.python.org/3/library/ast.html to Rascal}
@see{lang::python::Parse}
@author{Jurgen J. Vinju - Centrum Wiskunde & Informatica}
module lang::python::AST

extend util::Maybe;

alias Identifier = str;

data Module 
  = \module(list[Statement] body, list[TypeIgnore] typeIgnores)
  | \interactive(list[Statement] body)
  | \expression(Expression expr)
  | \functionType(list[Expression] argTypes, Expression returns)
  ;

data Statement(loc src=|unknown:///|)
  = functionDef(Identifier name, Arguments formals, list[Statement] body, list[Expression] decorators, Maybe[Expression] returns, Maybe[str] typeComment)
  | asyncFunctionDef(Identifier name, Arguments formals, list[Statement] body, list[Expression] decorators, Maybe[Expression] returns, Maybe[str] typeComment)
  | classDef(Identifier name, list[Expression] bases, list[Keyword] keywords, list[Statement] body, list[Expression] decorators)
  | \return(Maybe[Expression] optValue)
  | delete(list[Expression] targets)
  | assign(list[Expression] targets, Expression \val, Maybe[str] typeComment)
  | addAssign(Expression target, Expression val) 
  | subAssign(Expression target, Expression val) 
  | multAssign(Expression target, Expression val) 
  | matmultAssign(Expression target, Expression val) 
  | \divAssign(Expression target, Expression val) 
  | \modAssign(Expression target, Expression val) 
  | \powAssign(Expression target, Expression val) 
  | lshiftAssign(Expression target, Expression val)
  | rshiftAssign(Expression target, Expression val) 
  | bitorAssign(Expression target, Expression val) 
  | bitxorAssign(Expression target, Expression val) 
  | bitandAssign(Expression target, Expression val) 
  | floordivAssign(Expression target, Expression val)
  | annAssign(Expression target, Expression annotation, Maybe[Expression] optValue, bool simple)
  | \for(Expression target, Expression iter, list[Statement] body, list[Statement] orElse, Maybe[str] typeComment)
  | asyncFor(Expression target, Expression iter, list[Statement] body, list[Statement] orElse, Maybe[str] typeComment)
  | \while(Expression \test, list[Statement] body, list[Statement] orElse)
  | \if(Expression \test, list[Statement] body, list[Statement] orElse)
  | with(list[WithItem] items, list[Statement] body, Maybe[str] typeComment)  
  | asyncWith(list[WithItem] items, list[Statement] body, Maybe[str] typeComment)
  | raise(Maybe[Expression] exc, Maybe[Expression] cause)
  | \try(list[Statement] body, list[ExceptHandler] handlers, list[Statement] orElse, list[Statement] finalBody)
  | \assert(Expression \test, Maybe[Expression] msg)
  | \import(list[Alias] aliases)
  | importFrom(Maybe[Identifier] \module, list[Alias] aliases, Maybe[int] level)
  | global(list[Identifier] names)
  | nonlocal(list[Identifier] names)
  | expr(Expression \value)
  | pass()
  | \break()
  | \continue()
  ;

data Expression(loc src=|unknown:///|)
  = and(list[Expression] values)
  | or(list[Expression] values)
  ;

// Binary operators
data Expression
  = add(Expression lhs, Expression rhs) 
  | sub(Expression lhs, Expression rhs) 
  | mult(Expression lhs, Expression rhs) 
  | matmult(Expression lhs, Expression rhs) 
  | \div(Expression lhs, Expression rhs) 
  | \mod(Expression lhs, Expression rhs) 
  | \pow(Expression lhs, Expression rhs) 
  | lshift(Expression lhs, Expression rhs)
  | rshift(Expression lhs, Expression rhs) 
  | bitor(Expression lhs, Expression rhs) 
  | bitxor(Expression lhs, Expression rhs) 
  | bitand(Expression lhs, Expression rhs) 
  | floordiv(Expression lhs, Expression rhs)
  | invert(Expression operand) 
  | \not(Expression operand) 
  | uadd(Expression operand) 
  | usub(Expression operand)
  ;

data Expression
  = lambda(Arguments formals, Expression body)
  | namedExpr(Expression target, Expression \value)
  | ifExp(Expression \test, Expression body, Expression orelse)
  | dict(list[Expression] keys, list[Expression] values)
  | \set(list[Expression] elts)
  | listComp(Expression elt, list[Comprehension] generators)
  | setComp(Expression elt, list[Comprehension] generators)
  | dictComp(Expression key, Expression \value, list[Comprehension] generators)
  | generatorExp(Expression elt, list[Comprehension] generators)
  | await(Expression \value)
  | yield(Maybe[Expression] optValue)
  | yieldFrom(Expression \value)
  | compare(Expression lhs, list[CmpOp] ops, list[Expression] comparators)
  | call(Expression func, list[Expression] args, list[Keyword] keywords)
  | formattedValue(Expression \value, Maybe[Conversion] conversion, Maybe[Expression] formatSpec)
  | joinedStr(list[Expression] values)
  | constant(Constant \const, Maybe[str] kind)
  ;


// The following expression can appear only in assignment context  
data Expression
  = attribute(Expression \value, Identifier attr, ExprContext ctx)
  | subscript(Expression \value, Expression slice, ExprContext ctx)
  | starred(Expression \value, ExprContext ctx)
  | name(Identifier id, ExprContext ctx)
  | \list(list[Expression] elts, ExprContext ctx)
  | \tuple(list[Expression] elts, ExprContext ctx)
  ;

// Can appear only in Subscript
data Expression 
  = \slice(Maybe[Expression] lower, Maybe[Expression] upper, Maybe[Expression] step)
  ;

data ExprContext 
  = \load() 
  | store() 
  | del()
  ;

data Conversion 
  = noFormatting()
  | stringFormatting()
  | reprFormatting()
  | asciiFormatting()
  ;

data CmpOp 
  = eq() 
  | noteq() 
  | lt() 
  | lte() 
  | gt() 
  | gte() 
  | is() 
  | isnot() 
  | \in() 
  | \notin()
  ;

data Comprehension = comprehension(Expression target, Expression iter, list[Expression] ifs, bool isAsync);

data ExceptHandler(loc src = |unknown:///|) 
  = exceptHandler(Maybe[Expression] \type, Maybe[Identifier] optName, list[Statement] body);

data Arguments 
  = arguments(
      list[Arg] posonlyargs, 
      list[Arg] args, 
      Maybe[Arg] varargs, 
      list[Arg] kwonlyargs, 
      list[Expression] kw_defaults, 
      Maybe[Arg] kwarg, 
      list[Expression] defaults
  );

data Arg(loc src = |unknown:///|) 
  = arg(Identifier arg, Maybe[Expression] annotation, Maybe[str] typeComment);

data Keyword(loc src = |unknown:///|) 
  = \keyword(Identifier arg, Expression \value);

data Alias 
  = \alias(Identifier name, Maybe[Identifier] asName);

data WithItem 
  = withItem(Expression contextExpr, Maybe[Expression] optionalVars);

data TypeIgnore 
  = typeIgnore(int lineno, str \tag);

data Constant
  = none()
  | number(num n)
  | string(str s)
  | \tupleConst(list[Constant] elts)
  | \setConst(list[Constant] elts)
  | \listConst(list[Constant] elts)
  | \dictConst(list[Constant] keys, list[Constant] values)
  ;