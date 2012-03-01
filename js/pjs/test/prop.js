anObject = {
    a: 5,
    b: 6
};

f = fork(function(o) { return o.a + o.b; }, anObject);
oncompletion(function() {
    assert(f.get() == 11, "f.get() == " + f.get())
})
