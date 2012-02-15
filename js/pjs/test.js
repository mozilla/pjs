let i = 0;
let completions = [];

while (i++ < 50) {
    completions.push(fork(function fib(l) {
		let x = 1,
			y = 1,
			t = 0,
			i = 0;

		while(i++ < l) {
			t = y;
			y = x + y;
			x = t;
		}
		return ['fib', l, x];
	}, i));

    completions.push(fork(function phi(l) {
		let x = 1,
			i = 0;

		while (i++ < l) {
			x = x * 1.618;
		}
		return ['phi', i, x];
	}, i));
}

oncompletion(function() {
	for (var i = 0; i < completions.length; i++) {
		let [name, index, val] = completions[i].get();
		print(name, index, val);
	}
});
