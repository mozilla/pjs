a = [ 0, 1, 2, 3 ];

b = new Object();
b.__defineGetter__("p", function() {
	a[1]++;
	return a[1];
});

function fun1(obj) {
	return obj[3]; // no proxy should be used here.
}

f1 = fork(function(obj) {
	return wrap(fun1, obj); // no proxy should be used here.
}, a);

function fun2(obj) {
	obj[0] = 4; // rejected during the analysis.
	return 0;
}

f2 = fork(function(obj) {
	return wrap(fun2, obj);
}, a);

function fun3(obj) {
	return obj.p; // proxied and error is printed during runtime.
}

f3 = fork(function(obj) {
	return wrap(fun3, obj);
}, b);

oncompletion(function() {
	print(f1.get());
	print(f2.get()); // should return an error or null.
	print(f3.get()); // should return an error or null.
	print(a[0]);
	print(a[1]);
});
