function concatSorted(left, right)
{
	var temp = [];
	while(true)
	{
		if(left.len() == 0 && right.len() == 0) break;
		
		if(left.len() == 0)
		{
			temp = temp.concat(right);
			break;
		}
		
		if(right.len() == 0)
		{
			temp = temp.concat(left);
			break;
		}
		
		if(left.head() < right.head())
		{
			temp = temp.concat(left.head());
			left = left.tail();
		}
		else 
		{
			temp = temp.concat(right.head());
			right = right.tail();
		}
	}
	
	return temp;
}

function mergesort(x, low, high)
{
	if(low == high) return [x[low]]; //caso não façamos assim, ele irá retornar um inteiro

	var half = (low + high) / 2;
	var left = mergesort(x, low, half);
	var right = mergesort(x, half + 1, high);

	return concatSorted(left, right);
}

var x = [2, 1, 4, 3, 9, 5, 0, 7, 8];
//var x = [1];
mergesort(x, 0, x.len()-1);



