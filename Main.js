var x = 1, y = 10, w = 25, t = 0;
var z = [ x , y, y];
if (x < y){
break;
x = 100;
}

while (x < 10){
  y = y + 1;
  x = x + 1;
  break;
  while (y < 20){
  	x = 1000;
  }
}

switch(w){
case 20:
	t = 200;
	break;
case 10:
	t = 300;
	break;
default:
	t = 400;
  }

t = -(w) + 100;

for (var s = 1; s < 3; s++){
	t = 500;
}

function fat(x){
	if(x<=1){
		return 1;
	}
	return (fat(x-1))*x;
}

return fat(25);
