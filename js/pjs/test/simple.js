a = [ 0, 1, 2, 3 ];

b = new Object();
b.__defineGetter__("p", function() {
	a[1]++;
	return a[1];
});

f1 = fork(function(obj) {
	return obj[3]; // no proxy should be used here.
}, a);

f2 = fork(function(obj) {
	obj[0] = 4; // rejected during the analysis.
	return 0;
}, a);

f3 = fork(function(obj) {
	return obj.p; // proxied and error is printed during runtime.
}, b);

// XXX --- would require proxy due to indirect prop access
// f4 = fork(function(obj) {
// 	return obj[0].p;
// }, [b]);

//f5 = fork(function(obj) {
//	foo = {};
//	foo.p = 6;
//}, [b]);
//
//f5 = fork(function(bool, obj) {
//	foo = {};
//	if (bool) { foo = obj; }
//	foo.p = 6;
//}, true, [b]);

oncompletion(function() {
	print(f1.get());
	print(f2.get()); // should return an error or null.
	print(f3.get()); // should return an error or null.
	print(a[0]);
	print(a[1]);
});
