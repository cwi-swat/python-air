module lang::xml::\junit-report::TestSuites

import DateTime;

data TestSuites 
	= testsuites(
	    Properties properties,
		list[TestSuite] suites, 
		int disabled=0, 
		int errors=0, 
		int failures=0, 
		str name="test suite", 
		int tests=0, 
		int time=0
	);

data Properties 
	= properties(list[Property] properties);
	
data Property 
	= property(str name="", str \value="");
	
data TestSuite 
	= testsuite(
		list[TestCase] cases, 
		str name="test suite", 
		int tests=0, 
		int disabled=0, 
		int errors=0, 
		int failures=0, 
		str hostname="localhost", 
		int id=0,
		str package="",
		int skipped=0,
		int time=0,
		datetime timestamp=now()
	);

data TestCase 
	= testcase(TestResult result, str name="", int assertions=0, str classname="", int time=0);

data TestResult 
	= skipped(str message)
	| error(str text, str message="", str \type="")
	| failure(str text, str message="", str \type="")
	| \system-out(str text)
	| \system-err(str text)
	;   

