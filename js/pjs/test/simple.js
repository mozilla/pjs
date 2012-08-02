a = [0,1,2,3];

f1 = fork(function(){
	return a[3];
});

f2 = fork(function() {
	a[0] = 4;
	return 0;
});

oncompletion(function() {
	print(f1.get());
	print(f2.get()); // should return an error.
	print(a[0]);
})
