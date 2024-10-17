
## 0.0.7

* maintenance release to be up to date with latest rascal version


## 0.0.1, 0.0.2

* python-air wraps "libast" from the Python standard library to parse Python code.
  * generates a Python program that writes an AST as JSON to disk
  * executes that Python program using util::ShellExec::exec
  * reads the JSON and normalizes it towards a Rascal AST shape
  * takes care to retain position information on all AST nodes
* lang::xml::junit contains XML bindings for JUnit test suite reports.
  * this should probably be in the standard library in the future.
* this is quite experimental for now.
