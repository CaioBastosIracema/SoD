#Desafio de gestor por sexo

linguagem <-function(){
  for(x in 351:356){

    colnames(SoDfiltrado)[x]<-'linguagem'

    aux=SoDfiltrado%>%
      dplyr::filter(!is.na(`linguagem`))%>%
      dplyr::count(`('P1_b ', 'Genero')`,`linguagem`)%>%
      dplyr::group_by(`('P1_b ', 'Genero')`)%>%
      dplyr::mutate(perc = n/sum(n)*100)%>%
      dplyr::arrange(`('P1_b ', 'Genero')`)%>%.[,-3]

    colnames(aux)<-c('Gênero', paste('linguagem_',x), paste('perc_',x))

    if(x==351){
      lingua=aux
    }else{
      lingua=cbind(lingua, aux[,2:3])
    }
    SoDfiltrado=SoDfiltrado[,-x]
  }
  return(lingua)
}
