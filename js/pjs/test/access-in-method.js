anObject = {
    a: 5,
    get_a: function() { return this.a; }
};

f0 = fork(function(o) {
    return o.get_a();
}, anObject);

f1 = fork(function(o) {
    return o.get_a() + 1;
}, anObject);

oncompletion(function() {
	print(f0.get());
	assert(f0.get() == 5, "f1.get() == " + f0.get())
	print(f1.get());
	assert(f1.get() == 6, "f1.get() == " + f1.get())	
})
