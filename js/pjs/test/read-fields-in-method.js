var anObject = {
    a: 1, b: 2,
    sum: function() {
        return this.a + this.b;
    }
};

var expected = anObject.sum();
var task = fork(function() {
    return anObject.sum();
});
oncompletion(function() {
    print("expected = " + expected);
    var actual = task.get();
    print("actual = " + actual);
    assert(expected == actual, "expected == actual");
});