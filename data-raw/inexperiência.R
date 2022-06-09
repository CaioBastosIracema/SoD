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

SoDinexperiente%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
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

SoDinexperiente%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%c('Análise de Dados',
                                            'Ciência de Dados'))%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)

#mulheres tem 3.4 analistas para cada cientista
#homens tem 2.3 analistas para cada cientista

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
SoDinexperiente%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`) & !is.na(`('P2_g ', 'Nivel')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ggplot2::facet_wrap(~`('P2_g ', 'Nivel')`)

####################################################################################

#Filtrando a base para ter, apenas, pessoas junior
SoDjunior=SoDinexperiente%>%dplyr::filter(`('P2_g ', 'Nivel')` %in%c(
  'Júnior'
))

#Média de idade por gênero
SoDjunior%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))

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

#Formação por gênero
SoDjunior%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
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

###################################################################################
# Mulheres junior x Homens pleno

MJHP<-SoDinexperiente%>%
  dplyr::filter(`('P2_g ', 'Nivel')` %in%c('Pleno') &
                  `('P1_b ', 'Genero')`=='Masculino')

MJHP=SoDjunior%>%dplyr::filter(`('P1_b ', 'Genero')`=='Feminino')%>%
  rbind(MJHP)

#Média de idade por gênero
MJHP%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))


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

##############################################################################
################################################################################

#Filtrando a base para ter, apenas, analistas de dados
SoDanalista=SoDinexperiente%>%dplyr::filter(`('P4_a ', 'Atuacao')` %in%c(
  'Análise de Dados'
))

#Média de idade por gênero
SoDanalista%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))

#Nível de ensino por gênero
SoDanalista%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível por gênero
SoDanalista%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_g ', 'Nivel')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Formação por gênero
SoDanalista%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

########################################################################

###################################################################################

# Mulheres analistas x Homens cientistas

MAHC<-SoDinexperiente%>%dplyr::filter(`('P4_a ', 'Atuacao')` %in%c(
  'Ciência de Dados') & `('P1_b ', 'Genero')`=='Masculino')

MAHC=SoDanalista%>%dplyr::filter(`('P1_b ', 'Genero')`=='Feminino')%>%
  rbind(MAHC)

#Média de idade por gênero
MAHC%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))


#Nível de ensino por gênero
MAHC%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

