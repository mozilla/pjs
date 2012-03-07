function fib(fib, n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        let f1 = fork(fib, fib, n-1);
        let f2 = fork(fib, fib, n-2);
        oncompletion(function() {
            return f1.get() + f2.get();
        });
    }
}

f5 = fork(fib, fib, 5);
f6 = fork(fib, fib, 6);
f7 = fork(fib, fib, 7);
oncompletion(function() {
    print(f5.get())
    assert(f5.get() == 5, "f5.get() == " + f5.get())
    print(f6.get())
    assert(f6.get() == 8, "f6.get() == " + f6.get())
    print(f7.get())
    assert(f7.get() == 13, "f7.get() == " + f7.get())
})
