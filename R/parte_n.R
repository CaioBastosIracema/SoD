parte_n<-function(n){

  `%>%`=magrittr::`%>%`

  if(is.numeric(n)==T && n>=1 && n<=9){
    P<-readr::read_csv('data-raw/State of Data 2021 - Dataset - Pgina1.csv')%>%
      dplyr::select(dplyr::starts_with(paste("('P",n, sep = '')))

    return(P)
  }

  else{

    return()
  }
}
