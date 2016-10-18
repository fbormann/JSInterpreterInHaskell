var w = 0;
var y = 3;
var z = 6;
var t = [];

function addSomething(value){
	for(var i =0; i < 3; i++){
		w = 0;
		while(w < 3){
			value = value + w;
			w++;
		}
	}
}

var w = addSomething(y);
if(w > y*z){
	t.concat([z]);
}else{
	t.concat([y]);
}

y = t.head();
var tamanho = t.len();