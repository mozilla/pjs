anObject = {
    a: 5,
    set_a: function(a) { this.a = a; }
};

f = fork(function(o) {
    o.set_a(10);
}, anObject);

oncompletion(function() {
})
