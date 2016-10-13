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

return t;
