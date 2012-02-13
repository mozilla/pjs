let i = 0;
let completions = [];
while (i < 100) {
    print("hi");

    map = { a: 22, b: 23 };

    completions.push(fork(function(m, i) { print("in_fork 1", i); return [i, m.a]; }, map, i));
    completions.push(fork(function(m, i) { print("in_fork 2", i); return [i, m.b]; }, map, i));
    oncompletion(function() {
        print("in_completion");
        print("results: ");
		for (var i = 0; i < completions.length; i++) {
			print(completions[i].get());
		}
    });
    i++;
}
