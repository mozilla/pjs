//FIXME *Eventually*, this test should pass or at least give a reasonable error.

fut1 = fork(function() { return 10; });
fut2 = fork(function(fut1) { return fut1.get(); }, fut1);
oncompletion(function() {
    print(fut1.get());
    print(fut2.get());
});