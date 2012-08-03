function printProp(obj, objname) {
	for ( var prop in obj) {
		// print("obj[" + prop + "] = " + obj[prop]);
		// print(typeof Object.getOwnPropertyDescriptor(obj, prop));
		// print(typeof Object.getPrototypeOf(obj, prop));
		// print(Object.getPrototypeOf(obj, prop));
		print(objname + "[" + prop + "] " /*
											 * + test.__lookupGetter__(prop) + "
											 * of type " + (typeof
											 * test.__lookupGetter__(prop))
											 */ + ": has prototype "
											  + (Object.getPrototypeOf(obj, prop) != ""));
	}
}

Thing = function() {
};

a = 0;
Thing.prototype.__defineGetter__(8, function() {
	return ++a;
});

instance = new Thing;

instance.__defineGetter__(9, function() {
	return ++a;
});

function Test() {
	 return { get 8() { return ++a; }};
}

var b = 3;

test = new Test;

anarray = [];
anarray[8] = 88;
asecondarray = new Array();
asecondarray[8] = 888;

printProp(instance, "instance");
printProp(test, "test");
printProp(anarray, "anarray");
printProp(asecondarray, "asecondarray");

f1 = fork(function(obj) {
	// Code to be inserted.
//	pjs_analyze(this, arguments);
//	v = 6;
	obj[0] = 5;
 	return obj[8];
}, instance);

//f2 = fork(function(obj) {
//	// Code to be inserted.
//	pjs_analyze(this, arguments);
//	return obj[8];
//}, test);
//
f3 = fork(function(obj) {
	// Code to be inserted.
//	pjs_analyze(this, arguments);
	obj[0] = 5;
	return obj[8];
}, anarray);
//
//f4 = fork(function(obj) {
//	// Code to be inserted.
//	pjs_analyze(this, arguments);
//	return obj[8];
//}, asecondarray);

oncompletion(function() {
	print(f1.get());
//	print(f2.get());
	print(f3.get());
//	print(f4.get());
	print(instance[0]);
	print(anarray[0]);
	
	print(a);
//	print(b);
});
