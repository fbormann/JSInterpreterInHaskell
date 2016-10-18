function len(array) {
    
    var tam = 0;
    var temp = array;
    if(array.equals([])){
        return 0;
    }
    while (!temp.tail().equals([])){
        tam = tam + 1;
        temp = temp.tail();
    }
    return tam + 1;
}
function esquerda(array){
    tamanho = len(array) / 2;
    temp = array;
    var temp2 = [];
    while (tamanho > 0){
        temp2 = temp2.concat(temp.head());
        temp = temp.tail();
        tamanho--;
    }
    return temp2;
}
function direita(array){
    tamanho = len(array) / 2;
    temp = array;
    while (tamanho > 0){
        temp = temp.tail();
        tamanho--;
    }
    return temp;
}
function merge(x, y){
    var temp = [];
    while(true){
        if(len(x) == 0 && len(y) == 0) break;
        
        if(len(x) == 0){
            temp = temp.concat(y);
            break;
        }
        if(len(y) == 0){
            temp = temp.concat(x);
            break;
        }
        if(x.head < y.head){
            temp = temp.concat(x.head);
            x = x.tail;
        } else {
            temp = temp.concat(y.head);
            y = y.tail;
        }
    }
    
    return temp;
}
function mergesort(y){
    if(len(y) <= 1) return [y[0]];
    var left = mergesort(direita(y));
    var right = mergesort(esquerda(y));
    return merge(left, right);
}
var x = [8,7,6,5,4,3,2,1,0];
return mergesort(x);