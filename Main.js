vetor = [5,2,3,6,7,3,5,6,4,2,559,8,7,4,1,24,5,4,7,26,89];
for(var i = 0;i<vetor.len();i = i+1){
	for(var j = 0;j<vetor.lent();j = j+1){
		if(vetor[i]<vetor[j]){
			aux = vetor[i];
			vetor[i] = vetor[j];
			vetor[j] = aux;
		}
	}
}
return vetor;
