instance = new Object();

a = 0;

instance[8] = a++;

f = fork(function() {
	return instance[8];
});

oncompletion(function() {
	print(f.get());
	print(a);
});
