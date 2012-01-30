let i = 0;
while (i < 100) {
    print("hi");

    map = { a: 22, b: 23 };

    foo = fork(function(m) { print("in_fork 1"); return m.a; }, map);
    bar = fork(function(m) { print("in_fork 2"); return m.b; }, map);
    oncompletion(function() {
        print("in_completion");
        print("result of foo: ");
        print(foo.get());
        print("result of bar: ");
        print(bar.get());
    });
    i++;
}
