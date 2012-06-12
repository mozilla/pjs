function fn(n) {
	return 0;
}

f0 = fork(function() {
	return fn(0);
});

f1 = fork(function() {
	return fn(0);
});

oncompletion(function() {
})
