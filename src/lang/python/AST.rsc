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
  = functionDef(Identifier name, Arguments args, list[Statement] body, list[Expression] decorators, Maybe[Expression] returns, Maybe[str] typeComment)
  | asyncFunctionDef(Identifier name, Arguments args, list[Statement] body, list[Expression] decorators, Maybe[Expression] returns, Maybe[str] typeComment)
  | classDef(Identifier name, list[Expression] bases, list[Keyword] keywords, list[Statement] body, list[Expression] decorators)
  | \return(Maybe[Expression] \value)
  | delete(list[Expression] targets)
  | assign(list[Expression] targets, Expression \val, Maybe[str] typeComment)
  | augAssign(Expression target, Operator op, Expression \val)
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
  | \import(list[Alias] names)
  | importFrom(Maybe[Identifier] \module, list[Alias] names, Maybe[int] level)
  | global(list[Identifier] names)
  | nonlocal(list[Identifier] names)
  | expr(Expression \value)
  | pass()
  | \break()
  | \continue()
  ;

data Expression(loc src=|unknown:///|)
  = boolop(BoolOp boolop, list[Expression] values)
  | namedExpr(Expression target, Expression \value)
  | binOp(Expression \left, Operator binop, Expression \right)
  | unaryOp(UnaryOp unaryOp, Expression operand)
  | lambda(Arguments args, Expression body)
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
  | compare(Expression \left, list[CmpOp] ops, list[Expression] comparators)
  | call(Expression func, list[Expression] args, list[Keyword] keywords)
  | formattedValue(Expression \value, Maybe[int] conversion, Maybe[Expression] formatSpec)
  | joinedStr(list[Expression] values)
  | constant(Constant \const, Maybe[str] kind)
//          -- the following expression can appear in assignment context  
  | attribute(Expression \value, Identifier attr, ExprContext ctx)
  | subscript(Expression \value, Expression slice, ExprContext ctx)
  | starred(Expression \value, ExprContext ctx)
  | name(Identifier id, ExprContext ctx)
  | \list(list[Expression] elts, ExprContext ctx)
  | \tuple(list[Expression] elts, ExprContext ctx)
//          -- can appear only in Subscript
  | \slice(Maybe[Expression] lower, Maybe[Expression] upper, Maybe[Expression] step)
  ;

data ExprContext 
  = \load() 
  | store() 
  | del()
  ;

data BoolOp 
  = and() 
  | or()
  ;

data Operator 
  = add() 
  | sub() 
  | mult() 
  | matmult() 
  | \div() 
  | \mod() 
  | \pow() 
  | lshift()
  | rshift() 
  | bitor() 
  | bitxor() 
  | bitand() 
  | floordiv()
  ;

data UnaryOp 
  = invert() 
  | \not() 
  | uadd() 
  | usub()
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
  = exceptHandler(Maybe[Expression] \type, Maybe[Identifier] name, list[Statement] body);

data Arguments 
  = arguments(list[Arg] posonlyargs, list[Arg] args, Maybe[Arg] varargs, list[Arg] kwonlyargs, list[Expression] kw_defaults, Maybe[Arg] kwarg, list[Expression] defaults);

data Arg(loc src = |unknown:///|) 
  = arg(Identifier arg, Maybe[Expression] annotations, Maybe[str] typeComment);

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