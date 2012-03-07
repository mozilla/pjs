anObject = {
    a: 5,
    b: 6
};

f = fork(function(o) {
    o.a = 10;
    return 12;
}, anObject);

oncompletion(function() {
    assert(f.get() == 11, "f.get() == " + f.get())
})
