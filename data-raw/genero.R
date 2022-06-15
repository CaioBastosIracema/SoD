### Preparando a base####################################

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
    `('P1_b ', 'Genero')`!='Outro' )%>%
  dplyr::mutate(`('P2_h ', 'Faixa salarial')`= forcats::lvls_reorder(
  as.factor(`('P2_h ', 'Faixa salarial')`),
  c(13, 2, 5, 8, 10, 11, 12, 3, 4, 6, 7, 9, 1)),
  `('P1_h ', 'Nivel de Ensino')`= forcats::lvls_reorder(
    as.factor(`('P1_h ', 'Nivel de Ensino')`), c(5, 2, 3, 6, 4 , 1, 7)),
  `('P2_i ', 'Quanto tempo de experiência na área de dados você tem?')`= forcats::lvls_reorder(
    as.factor(`('P2_i ', 'Quanto tempo de experiência na área de dados você tem?')`),
    c(7, 6, 1, 2, 3, 4, 5)),
  `('P2_j ', 'Quanto tempo de experiência na área de TI/Engenharia de Software você teve antes de começar a trabalhar na área de dados?')`
  = forcats::lvls_reorder(
    as.factor(`('P2_j ', 'Quanto tempo de experiência na área de TI/Engenharia de Software você teve antes de começar a trabalhar na área de dados?')`
),
    c(7, 6, 1, 2, 3, 4, 5))
)
#alterando nomes de colunas para simplificar
colnames(SoDfiltrado)[c(20,21)]=c('Experiência na área de dados',
                                  'Experiência na área de TI')

#renomeando categorias
SoDfiltrado=SoDfiltrado%>%dplyr::mutate(`Experiência na área de dados`=forcats::lvls_revalue(
  `Experiência na área de dados`, c('Sem experiência', 'Menos de 1 ano',
                                    '1 a 2 anos', '2 a 3 anos', '4 a 5 anos',
                                    '6 a 10 anos', 'Mais de 10 anos'))
)

SoDfiltrado=SoDfiltrado%>%dplyr::mutate(`Experiência na área de TI`=forcats::lvls_revalue(
  `Experiência na área de TI`, c('Sem experiência', 'Menos de 1 ano',
                                    '1 a 2 anos', '2 a 3 anos', '4 a 5 anos',
                                    '6 a 10 anos', 'Mais de 10 anos'))
)

###############################################################################
#Pessoas por gênero

SoDfiltrado%>%dplyr::count(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)

#Média de idade por gênero
SoDfiltrado%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)

SoDfiltrado%>%
  dplyr::group_by(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))%>%
  dplyr::arrange(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)

SoDfiltrado%>%
  dplyr::group_by(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))%>%
  dplyr::arrange(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)

SoDfiltrado%>%
  dplyr::group_by(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::summarise(idade= mean(`('P1_a ', 'Idade')`, na.rm=T))%>%
  dplyr::arrange(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)


#Região por gênero
 SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_e_b ', 'Regiao onde mora')`) %>%
   dplyr::group_by(`('P1_b ', 'Genero')`) %>%
   dplyr::mutate(Prop = n/sum(n))%>%
ggplot2::ggplot(
  ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
      fill = `('P1_e_b ', 'Regiao onde mora')`)) +
   ggplot2::geom_col( color="white",
                      position = ggplot2::position_fill()) +
   ggrepel::geom_label_repel(ggplot2::aes(
     label = scales::percent(Prop)),
     fontface = 'bold',
     hjust=2,
     position = ggplot2::position_stack(vjust = .5),
       size=3.5) +
   ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero e Região')+
   ggplot2::theme_minimal()+ggplot2::xlab('Gênero')+
   ggplot2::theme(legend.position="bottom",
                  plot.title=ggplot2::element_text(face='bold.italic',
                                                   hjust = 0.5, size=20),
                  axis.text.y=ggplot2::element_blank(),
                  axis.title.y=ggplot2::element_blank(),
                  axis.title.x=ggplot2::element_blank(),
                  axis.text.x =ggplot2::element_text(face='bold', size=12),
                  legend.title=ggplot2::element_blank())

#Atuação por gênero
SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%notin%c('Outra',
                                               'Buscando emprego na área de dados.') &
                  !is.na(`('P4_a ', 'Atuacao')`))%>%
  ggplot2::ggplot(ggplot2::aes( y=`('P1_b ', 'Genero')`) )+
  ggplot2::geom_bar(ggplot2::aes(
    fill=as.factor(`('P4_a ', 'Atuacao')`)),
    position="fill")+
  viridis::scale_fill_viridis(discrete = T, option = "E")+
  hrbrthemes::theme_ipsum()

######################################################################
# Nível de ensino

#Nível de ensino por gênero
SoDfiltrado%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes( y=`('P1_b ', 'Genero')`))+
  ggplot2::geom_bar(ggplot2::aes(fill=as.factor(`('P1_h ', 'Nivel de Ensino')`)),
                    position="fill")


#Nível de ensino por gênero e Nível de cargo
SoDfiltrado%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
                  !is.na(`('P2_g ', 'Nivel')`))%>%
  ggplot2::ggplot(ggplot2::aes( y=`('P1_b ', 'Genero')`))+
  ggplot2::geom_bar(ggplot2::aes(fill=as.factor(`('P1_h ', 'Nivel de Ensino')`)),
                    position="fill")+
  ggplot2::facet_wrap(~`('P2_g ', 'Nivel')`)

#Nível de ensino por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`,`('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
                  !is.na(`('P4_a ', 'Atuacao')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
                  !is.na(`('P4_a ', 'Atuacao')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#############################################################################
######Nível de cargo

#Nível de cargo por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_g ', 'Nivel')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível de cargo por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P2_g ', 'Nivel')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`,`('P2_g ', 'Nivel')`)%>%
  dplyr::filter(!is.na(`('P2_g ', 'Nivel')`) &
                  `('P4_a ', 'Atuacao')`%notin%c('Outra',
                                                 'Buscando emprego na área de dados.'))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`)%>%
  dplyr::filter(!is.na(`('P2_g ', 'Nivel')`) &
                  `('P4_a ', 'Atuacao')`%notin%c('Outra',
                                                 'Buscando emprego na área de dados.'))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)


#####################################################################
####### Experiência###############################

#experiência por gênero
expsex=SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `Experiência na área de dados`)%>%
dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
                   dplyr::filter(
                     !is.na(`Experiência na área de dados`)
                   )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de dados`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `Experiência na área de TI`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
          !is.na(`Experiência na área de TI`)
        )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de TI`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

gridExtra::grid.arrange(expsex, expsexTI)

#Experiência por gênero e nível de cargo
expsex=SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `Experiência na área de dados`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de dados`) & !is.na(`('P2_g ', 'Nivel')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de dados`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `Experiência na área de TI`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de TI`) & !is.na(`('P2_g ', 'Nivel')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de TI`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

gridExtra::grid.arrange(expsex, expsexTI)

#Experiência por gênero e nível de ensino
expsex=SoDfiltrado%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`,
               `Experiência na área de dados`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de dados`) &
      !is.na(`('P1_h ', 'Nivel de Ensino')`) &
      `('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar'
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de dados`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`,
               `Experiência na área de TI`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de TI`) &
     !is.na(`('P1_h ', 'Nivel de Ensino')`) &
      `('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar'
    )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de TI`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

gridExtra::grid.arrange(expsex, expsexTI)


#Experiência por gênero e atuação
expsex=SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `Experiência na área de dados`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de dados`) & `('P4_a ', 'Atuacao')`!='Outra'
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de dados`,
                               fill=`('P4_a ', 'Atuacao')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `Experiência na área de TI`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`Experiência na área de TI`) & `('P4_a ', 'Atuacao')`!='Outra'
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`Experiência na área de TI`,
                               fill=`('P4_a ', 'Atuacao')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

gridExtra::grid.arrange(expsex, expsexTI)

###############################################################
###Faixa Salarial########

#Faixa salarial por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Faixa salarial por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`) &
                  `('P4_a ', 'Atuacao')`!='Outra')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P4_a ', 'Atuacao')`))+
  ggplot2::geom_col(position = 'dodge')+ ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Faixa salarial por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`, `('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`, `('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`) &
                  !is.na(`('P2_g ', 'Nivel')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+ ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Faixa salarial por gênero e nível de ensino
SoDfiltrado%>%dplyr::filter()%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`) &
                  `('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Faixa salarial por gênero e experiência na área de dados
SoDfiltrado%>%
  dplyr::count(`Experiência na área de dados`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`Experiência na área de dados`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`Experiência na área de dados`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`Experiência na área de dados`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Faixa salarial por gênero e experiência na área de TI
SoDfiltrado%>%
  dplyr::count(`Experiência na área de TI`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`Experiência na área de TI`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`Experiência na área de TI`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`Experiência na área de TI`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Satisfação por sexo
SoDfiltrado%>%
  dplyr::count(
    `('P1_b ', 'Genero')`,`('P2_k ', 'Você está satisfeito na sua empresa atual?')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
    dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_k ', 'Você está satisfeito na sua empresa atual?')`))

########################################################################
##########################Formação ########################################

#Formação por gênero
SoDfiltrado%>%
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

#Formação por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
                               fill=`('P4_a ', 'Atuacao')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Formação por gênero e nível de ensino
SoDfiltrado%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

#Formação por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%
  dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`) &
      !is.na(`('P2_g ', 'Nivel')`)
  )%>%
  ggplot2::ggplot(ggplot2::aes(x=perc,
                               y=`('P1_i ', 'Área de Formação')`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')+
  ggplot2::facet_wrap(~`('P1_b ', 'Genero')`)

############################################################################
## Proporção cientista/analista

SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%c('Análise de Dados',
                                            'Ciência de Dados'))%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%dplyr::mutate(perc = n/sum(n)*100)
#mulheres tem 2.75 analistas para cada cientista
#homens tem 1.9 analistas para cada cientista

#Proporção cientista/analista por nível de cargo
SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%c('Análise de Dados',
                                            'Ciência de Dados'))%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`)%>%dplyr::mutate(perc = n/sum(n)*100)

#Proporção cientista/analista por nível de ensino
SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%
                c('Análise de Dados', 'Ciência de Dados') &
                `('P1_h ', 'Nivel de Ensino')`!='Não tenho graduação formal')%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`,
               `('P4_a ', 'Atuacao')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)

