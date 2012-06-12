function fib(n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        let fib1 = fork(fib, n-1);
        let fib2 = fork(fib, n-2);

        oncompletion(function() {
            return fib1.get() + fib2.get();
        });
    }
}

f0 = fork(fib, 0);
f1 = fork(fib, 1);
f2 = fork(fib, 2);
f3 = fork(fib, 3);
f4 = fork(fib, 4);
f5 = fork(fib, 5);
f6 = fork(fib, 6);
f7 = fork(fib, 7);

oncompletion(function() {
    print(f0.get())
    assert(f0.get() == 0, "f0.get() == " + f0.get())
    print(f1.get())
    assert(f1.get() == 1, "f1.get() == " + f1.get())
    print(f2.get())
    assert(f2.get() == 1, "f2.get() == " + f2.get())
    print(f3.get())
    assert(f3.get() == 2, "f3.get() == " + f3.get())
    print(f4.get())
    assert(f4.get() == 3, "f4.get() == " + f4.get())
    print(f5.get())
    assert(f5.get() == 5, "f5.get() == " + f5.get())    
    print(f6.get())
    assert(f6.get() == 8, "f6.get() == " + f6.get())
    print(f7.get())
    assert(f7.get() == 13, "f7.get() == " + f7.get())
})
