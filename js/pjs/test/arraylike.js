Thing = function() {};

a = 0;
Thing.prototype.__defineGetter__(8, function() {
	return a++;
});

instance = new Thing;


f = fork(function(){
	return instance[8];
});

oncompletion(function(){
	print(f.get());
	print(a);
});
