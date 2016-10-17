var x = 3;
function teste(w){
	return w + 3;
}
var y = 0;
switch(x){
	case 2:
		 y = 2;
	break;
	case 1:
		 y = 1;
	break;
	default:
		y = teste(x);
	break;

}

