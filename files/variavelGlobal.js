var x = 3;

function test(x){
	x = x + 5;
	return x + 4;
}

var w = test(x);


function multiplyBy(array, number){
	for(var i = 0; i < array.len();i++){
		array[i] = array[i]*number;
	}
	return array;
}

var s = [12,20,30];
s = multiplyBy(s, x);
