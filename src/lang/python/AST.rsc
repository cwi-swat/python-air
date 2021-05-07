@origin{This module was inspired by translating the code on https://docs.python.org/3/library/ast.html to Rascal}
@author{Jurgen J. Vinju - Centrum Wiskunde & Informatica}
module lang::python::AST

extend util::Maybe;

data Identifier = id(str name);

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
  | annAssign(Expression target, Expression annotation, Maybe[Expression] optValue, int simple)
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
  | formattedValue(Expression \value, Maybe[int] conversion, Maybe[Expression] formatSpec)
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

data CmpOp 
  = eq() 
  | noteq() 
  | lt() 
  | lte() 
  | gr() 
  | gte() 
  | is() 
  | isnot() 
  | \in() 
  | \notin()
  ;

data Comprehension = comprehension(Expression target, Expression iter, list[Expression] ifs, int isAsync);

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