
###Importando a base de dados##########################

SoD2021<-readr::read_csv('data/State of Data 2021 - Dataset - Pgina1.csv')
`%>%`=magrittr::`%>%`
`%notin%`=Negate(`%in%`)

#Preparando a base para as análises
#Incluo na base apenas as categorias "Empreendedor ou Empregado (CNPJ)" e
#"Empregado (CLT)" da variável "Qual sua situação atual de trabalho?".
#Excluo a categoria de gênero "Outros".
#Além disso, reordeno as categorias das variáveis "nível de ensino",
#"faixa salarial", "experiência na área de dados" e "experiência na área de TI"

SoDfiltrado=SoD2021%>%dplyr::filter(
  `('P2_a ', 'Qual sua situação atual de trabalho?')`%in%c(
    'Empreendedor ou Empregado (CNPJ)', 'Empregado (CLT)') &
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


#Substituindo os valores "NA"s da variável "Nível do cargo", pelo valor "Gestor"

SoDfiltrado$`('P2_g ', 'Nivel')`=replace(SoDfiltrado$`('P2_g ', 'Nivel')`,
        is.na(SoDfiltrado$`('P2_g ', 'Nivel')`), 'Gestor')

#Reordenando as categorias da variável "Nível do cargo"

SoDfiltrado= SoDfiltrado%>%dplyr::mutate(
  `('P2_g ', 'Nivel')`=forcats::lvls_reorder(
  as.factor(`('P2_g ', 'Nivel')`),
  c( 2, 3, 4, 1))
  )

#Criando a categoria "Acima de R$ 16.001/mês" em "faixa salarial"

levels(SoDfiltrado$`('P2_h ', 'Faixa salarial')`)[
  levels(SoDfiltrado$`('P2_h ', 'Faixa salarial')`)%in%c("de R$ 16.001/mês a R$ 20.000/mês",
                                                         "de R$ 20.001/mês a R$ 25.000/mês",
                                                         "de R$ 25.001/mês a R$ 30.000/mês",
                                                         "de R$ 30.001/mês a R$ 40.000/mês",
                                                         "Acima de R$ 40.001/mês")] <- "Acima de R$ 16.001/mês"


##Experiência#############

#Alterando o nome das variáveis "Experiência na área de dados" e
# "Experiência na área de TI" para simplificar a manipulação

colnames(SoDfiltrado)[c(20,21)]=c('Experiência na área de dados',
                                  'Experiência na área de TI')


#renomeando categorias das variáveis "Experiência na área de dados" e
# "Experiência na área de TI"

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
   ggplot2::theme_void()+ggplot2::xlab('Gênero')+
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
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P4_a ', 'Atuacao')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Atuação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Spectral")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

######################################################################
# Nível de ensino

#Nível de ensino por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_h ', 'Nivel de Ensino')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "BuPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())


#Nível de ensino por gênero e Nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(!is.na(`('P2_g ', 'Nivel')`))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_h ', 'Nivel de Ensino')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "BuPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)


#Nível de ensino por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`!='Buscando emprego na área de dados.')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_h ', 'Nivel de Ensino')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "BuPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

#############################################################################
######Nível de cargo

#Nível de cargo por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_g ', 'Nivel')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Oranges")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

#Nível de cargo por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`!='Gestor')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_g ', 'Nivel')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Oranges")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

#Nível de cargo por gênero e nível de ensino
SoDfiltrado%>%
       dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
                    `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(!is.na(`('P2_g ', 'Nivel')`))%>%
      ggplot2::ggplot(ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                      fill = `('P2_g ', 'Nivel')`)) +ggplot2::geom_col(
                        color="white", position = ggplot2::position_fill()) +
ggrepel::geom_label_repel(ggplot2::aes(label = scales::percent(Prop)),
                          fontface = 'bold', hjust=2,
                          position = ggplot2::position_stack(vjust = .5),
                          size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Oranges")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic'),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
                   ggplot2::facet_grid(~`('P1_h ', 'Nivel de Ensino')`)


#####################################################################
####### Experiência###############################

#experiência por gênero
expsex=SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `Experiência na área de dados`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de dados`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Experiência na área de dados')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "RdPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `Experiência na área de TI`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de TI`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Experiência na área de TI')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "PuRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

gridExtra::grid.arrange(expsex, expsexTI)

#Experiência por gênero e nível de cargo
expsex=SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `Experiência na área de dados`) %>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de dados`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Experiência na área de dados e Nível de cargo')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "RdPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `Experiência na área de TI`) %>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de TI`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Experiência na área de TI e Nível de cargo')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "PuRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

expsex
expsexTI

#Experiência por gênero e atuação
expsex=SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `Experiência na área de dados`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`!='Outra')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de dados`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Experiência na área de dados e Atuação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "RdPu")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

expsexTI=SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `Experiência na área de TI`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`!='Outra')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `Experiência na área de TI`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Experiência na área de TI e Atuação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "PuRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

expsex
expsexTI

###############################################################
###Faixa Salarial########

#Faixa salarial por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero e Faixa Salarial')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "YlOrRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

#Faixa salarial por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%notin%c('Outra', 'Gestor'))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e Atuação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Greens")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

#Faixa salarial por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e Nível de cargo')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "YlOrRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

#Faixa salarial por gênero e nível de ensino
SoDfiltrado%>%
  dplyr::count(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e Nível de ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "YlOrRd")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P1_h ', 'Nivel de Ensino')`)

#Faixa salarial por gênero e experiência na área de dados
SoDfiltrado%>%
  dplyr::count(`Experiência na área de dados`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`Experiência na área de dados`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e Experiência na área de dados')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Greens")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`Experiência na área de dados`)

#Faixa salarial por gênero e experiência na área de TI
SoDfiltrado%>%
  dplyr::count(`Experiência na área de TI`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`Experiência na área de TI`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e Experiência na área de TI')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Greens")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`Experiência na área de TI`)

#Faixa salarial por gênero e situação de trabalho
SoDfiltrado%>%
  dplyr::count(`('P2_a ', 'Qual sua situação atual de trabalho?')`,
               `('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P2_a ', 'Qual sua situação atual de trabalho?')`,
                  `('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero,
                Faixa Salarial e situação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Greens")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_a ', 'Qual sua situação atual de trabalho?')`)

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
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_i ', 'Área de Formação')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`)) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_i ', 'Área de Formação')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero e Área de Formação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Pastel1")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())

#Formação por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_i ', 'Área de Formação')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero e Área de Formação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Pastel1")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

############################################################################
#Formação por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `('P1_i ', 'Área de Formação')`) %>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%dplyr::filter(
    !is.na(`('P1_i ', 'Área de Formação')`))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_i ', 'Área de Formação')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Pessoas Entrevistadas, por Gênero e Área de Formação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Pastel1")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

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

######################## Faixa de idade ###################

#Faixas de idade por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `('P1_a_a ', 'Faixa idade')`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%notin%c('Outra','Gestor'))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_a_a ', 'Faixa idade')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Faixa de idade e Atuação')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Reds")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

#Faixas de idade por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `('P1_a_a ', 'Faixa idade')`) %>%
  dplyr::group_by(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P2_g ', 'Nivel')`%notin%c('Outra'))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P1_a_a ', 'Faixa idade')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero,
                   Faixa de idade e Nível de Cargo')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Reds")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

####### Engenharia de dados  #####################

#Faixa salarial por gênero, Engenheiros de Dados Júnior e Pleno
SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%c('Engenharia de Dados') &
                  `('P2_g ', 'Nivel')`!='Sênior')%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`) %>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_h ', 'Faixa salarial')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::labs(title = 'Engenheiros de Dados Júnior e Pleno, por Gênero e
                Faixa Salarial')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Greens")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)



#Atuação por gênero e Nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P2_g ', 'Nivel')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P2_g ', 'Nivel')`!='Gestor')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P4_a ', 'Atuacao')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Spectral")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

#Situação por gênero e atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`,
               `('P2_a ', 'Qual sua situação atual de trabalho?')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P4_a ', 'Atuacao')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`!='Outra')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_a ', 'Qual sua situação atual de trabalho?')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Blues")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P4_a ', 'Atuacao')`)

#Situação por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`,
               `('P2_a ', 'Qual sua situação atual de trabalho?')`) %>%
  dplyr::group_by(`('P1_b ', 'Genero')`,`('P2_g ', 'Nivel')`) %>%
  dplyr::mutate(Prop = n/sum(n))%>%
  dplyr::filter(`('P2_g ', 'Nivel')`!='Outra')%>%
  ggplot2::ggplot(
    ggplot2::aes(x = `('P1_b ', 'Genero')`, y = Prop,
                 fill = `('P2_a ', 'Qual sua situação atual de trabalho?')`)) +
  ggplot2::geom_col( color="white",
                     position = ggplot2::position_fill()) +
  ggrepel::geom_label_repel(ggplot2::aes(
    label = scales::percent(Prop)),
    fontface = 'bold',
    hjust=2,
    position = ggplot2::position_stack(vjust = .5),
    size=3.5) +
  ggplot2::ggtitle('Pessoas Entrevistadas, por Gênero e Nível de Ensino')+
  ggplot2::theme_void()+
  ggplot2::scale_fill_brewer(type = "seq", palette = "Blues")+
  ggplot2::xlab('Gênero')+
  ggplot2::theme(legend.position="bottom",
                 plot.title=ggplot2::element_text(face='bold.italic',
                                                  hjust = 0.5, size=20),
                 axis.text.y=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.text.x =ggplot2::element_text(face='bold', size=12),
                 legend.title=ggplot2::element_blank())+
  ggplot2::facet_grid(~`('P2_g ', 'Nivel')`)

SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`,
               `('P2_a ', 'Qual sua situação atual de trabalho?')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(Percentual = n/sum(n)*100)%>%.[,-3]
