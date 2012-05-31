var temp = 2;

f0 = fork(function() {
	return temp + 1;
});

f1 = fork(function() {
	return temp + 2;
});

oncompletion(function() {
	print(f0.get());
	print(f1.get());
})
