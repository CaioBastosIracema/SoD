#Filtrando a base para ter, apenas, pessoas com até 3 anos de experiência
#na área de dados
SoDinexperiente=SoDfiltrado%>%dplyr::filter(`Experiência na área de dados` %in%c(
  'Menos de 1 ano','1 a 2 anos'
))

#Nível de ensino por gênero
SoDinexperiente%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Atuação por gênero
SoDinexperiente%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível de cargo por gênero
SoDinexperiente%>%dplyr::filter(!is.na(`('P2_g ', 'Nivel')`))%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_g ', 'Nivel')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#????????????????????????????????????????????????
#Faixa salarial por gênero e nível de cargo
SoDinexperiente%>%dplyr::filter(!is.na(`('P2_g ', 'Nivel')`))%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ggplot2::facet_wrap(~`('P2_g ', 'Nivel')`)

####################################################################################

#Filtrando a base para ter, apenas, pessoas junior
SoDjunior=SoDinexperiente%>%dplyr::filter(`('P2_g ', 'Nivel')` %in%c(
  'Júnior'
))

#Nível de ensino por gênero
SoDjunior%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Atuação por gênero
SoDjunior%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Faixa salarial por gênero e nível de ensino?????????
SoDjunior%>%dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ggplot2::facet_wrap(~`('P1_h ', 'Nivel de Ensino')`)


########################################################################

#Filtrando a base para ter, apenas, pessoas pleno
SoDpleno=SoDinexperiente%>%dplyr::filter(`('P2_g ', 'Nivel')` %in%c(
  'Pleno'
))

#Nível de ensino por gênero
SoDpleno%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Atuação por gênero
SoDpleno%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Faixa salarial por gênero e nível de ensino?????????
SoDpleno%>%dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ggplot2::facet_wrap(~`('P1_h ', 'Nivel de Ensino')`)

###################################################################################

MJHP<-SoDpleno%>%dplyr::filter(`('P1_b ', 'Genero')`=='Masculino')

MJHP=SoDjunior%>%dplyr::filter(`('P1_b ', 'Genero')`=='Feminino')%>%
  rbind(MJHP)

#Nível de ensino por gênero
MJHP%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

