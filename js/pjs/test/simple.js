a = [ 0, 1, 2, 3 ];

b = new Object();
b.__defineGetter__("p", function() {
	a[1]++;
	return a[1];
});

f1 = fork(function(obj) {
	return obj[3];
}, a);

f2 = fork(function(obj) {
	obj[0] = 4;
	return 0;
}, a);

f3 = fork(function(obj) {
	return obj.p;
}, b);

oncompletion(function() {
//	print(f1.get());
//	print(f2.get()); // should return an error.
//	print(f3.get());
	print(a[0]);
	print(a[1]);
});
