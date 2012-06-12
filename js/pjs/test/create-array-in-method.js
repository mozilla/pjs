function farr(val){
	var arr = new Array();
	arr.push(val);
	return arr;	
}

f0 = fork(farr,10);
f1 = fork(farr,20);

oncompletion(function() {
	print(f0.get()[0]);
	print(f1.get()[0]);
})
