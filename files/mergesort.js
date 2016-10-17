var items = [2,3,5,4,20,14,17,13];

function slice(elements, begin, end){
	var elements2 = [];
	for(var i = begin; i < end; i++){
		elements2[i-begin] = elements[i];
	}
	return elements2;
}

function mergeSort(items){

    // Terminal case: 0 or 1 item arrays don't need sorting
    if (items.len() < 2) {
        return items;
    }

    var middle = items.len() / 2,
        left    = items.slice(0, middle),
        right   = items.slice(middle);

    return merge(mergeSort(left), mergeSort(right));

}

items2 = mergeSort(items);