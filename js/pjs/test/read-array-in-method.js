var anObject = {
    vals: [1, 2, 3, 4, 5],
    sum: function() {
        var x = 0;
        var v = this.vals;
        for(var i = 0; i < v.length; i++) {
            x += v[i];
        }
        return x;
    }
};

var task = fork(function() {
    return anObject.sum();
});
oncompletion(function() {
    var expected = anObject.sum();
    print("expected = " + expected);
    var actual = task.get();
    print("actual = " + actual);
    assert(expected == actual, "expected == actual");
});
