SoD2021<-readr::read_csv('data/State of Data 2021 - Dataset - Pgina1.csv')
`%>%`=magrittr::`%>%`
`%notin%`=Negate(`%in%`)

#mice::md.pattern(SoD2021)
#c2<-densityplot(train$Age)

SoDfiltrado=SoD2021%>%dplyr::filter(
  `('P2_a ', 'Qual sua situação atual de trabalho?')`%notin%c(
    'Somente Estudante (graduação)', 'Somente Estudante (pós-graduação)',
    'Desempregado e não estou buscando recolocação',
    'Desempregado, buscando recolocação') &
    `('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
    `('P1_b ', 'Genero')`!='Outro' & !is.na(`('P2_h ', 'Faixa salarial')`))%>%
  dplyr::mutate(`('P2_h ', 'Faixa salarial')`= forcats::lvls_reorder(
  as.factor(`('P2_h ', 'Faixa salarial')`),
  c(13, 2, 5, 8, 10, 11, 12, 3, 4, 6, 7, 9, 1)),
  `('P1_h ', 'Nivel de Ensino')`= forcats::lvls_reorder(
    as.factor(`('P1_h ', 'Nivel de Ensino')`), c(5, 2, 3, 6, 4 , 1))

)

#Atuação por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                             fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Média de idade por gênero
SoDfiltrado%>%
  dplyr::group_by(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))%>%
  dplyr::arrange(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)

#Nível de ensino por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Faixa salarial por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Faixa salarial por gênero e atuacao
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ ggplot2::facet_wrap(~`('P4_a ', 'Atuacao')`)
