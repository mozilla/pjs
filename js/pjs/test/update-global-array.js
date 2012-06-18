// This test case should test that I cannot update an element in a parent array.
var temp = [1,2,3,4,5];
print(temp);
	
f = fork(function(a) {
	temp[0] = a;
}, 10);

oncompletion(function() {
	print(temp);
})
