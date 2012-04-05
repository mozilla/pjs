fut1 = fork(function() { return 10; });
fut2 = fork(function() { return fut1.get(); });
oncompletion(function() {
    print(fut1.get());
    print(fut2.get());
});
