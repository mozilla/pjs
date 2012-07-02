function printInt32Array(arr) {
	var temp = new Array();
	for ( var i = 0; i < arr.length; i++)
		temp.push(arr[i]);
	print(temp);
}

function merge(a, sizei, sizej) {
	var temp;
	var ii = 0;
	var ji = sizei;
	var flength = sizei + sizej;

	for ( var f = 0; f < (flength - 1); f++) {
		if (sizei == 0 || sizej == 0)
			break;

		if (a[ii] < a[ji]) {
			ii++;
			sizei--;
		} else {
			temp = a[ji];
			for ( var z = (ji - 1); z >= ii; z--)
				a[z + 1] = a[z];
			ii++;

			a[f] = temp;

			ji++;
			sizej--;
		}
	}
}

var somearray = new Int32Array([4,5]);

function bubblesort(a) {
	var len = a.length;
	for ( var i = 0; i < len; i++) {
		for ( var j = i + 1; j < len; j++) {
			if (a[i] > a[j]) {
				var temp = a[i];
				a[i] = a[j];
				a[j] = temp;
			}
		}
	}
	return len;
}

function mergesort_parallel(a) {
	resarr = forkN(2, bubblesort, a);
	
	oncompletion(function() {
		merge(a, resarr[0].get(), resarr[1].get());
		for(var i =0; i < size - 1; i++){
			if(a[i - 1] > a[i])
				print("unordered element at " + i);
		}
	});
}
var size = 1000;
function mergesort_parallel2(a) {
	resarr = fork(bubblesort, a);
	
	oncompletion(function() {
		print(resarr.get());
	});
}


my_array = new Int32Array(size);
for(var i =0; i < size; i++){
	my_array[i] = Math.floor(Math.random()*10001);
}

mergesort_parallel(my_array);