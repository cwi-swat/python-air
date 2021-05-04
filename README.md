# python-air

This is an *experimental* front-end for Python analysis in Rascal. It uses the Python `ast` library to create abstract
syntax trees for Python source files or Python code snippets, and maps those to a convenient `data` type in Rascal.

TODO list:
   * [ ] define `data` type for all possible AST nodes
   * [ ] call libast parser from Rascal
   * [ ] serialize Python AST objects to JSON
   * [ ] deserialize JSON AST objects into Rascal
   * [ ] simplify JSON objects to Rascal `data` constructors
   * [ ] add tests
   * [ ] add demo
