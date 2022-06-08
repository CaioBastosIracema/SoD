#Desafio de gestor por sexo

gestor<-function(){
  for(x in 59:69){
    colnames(SoDfiltrado)[x]='desafio'

    aux=SoDfiltrado%>%
      dplyr::filter(!is.na(`desafio`))%>%
      dplyr::count(
        `('P1_b ', 'Genero')`,`desafio`)%>%
      dplyr::group_by(`('P1_b ', 'Genero')`)%>%
      dplyr::mutate(perc = n/sum(n)*100)%>%
      dplyr::arrange(`('P1_b ', 'Genero')`)%>%.[,-3]

    colnames(aux)<-c('Gênero', paste('desafio_',x), paste('perc_',x))

    if(x==59){
      gestor=aux
    }else{
      gestor=cbind(gestor, aux[,2:3])
    }
    SoDfiltrado=SoDfiltrado[,-x]
  }
  return(gestor)
}
