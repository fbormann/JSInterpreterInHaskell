var i = 6;
function fibonacci(x){
	if(x == 2 || x == 1){
		return 1;
	}else{
		return fibonacci(x-1)+fibonacci(x-2);
	}
}
var w = fibonacci(i);