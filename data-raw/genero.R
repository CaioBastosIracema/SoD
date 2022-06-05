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

#Pessoas por gênero

SoDfiltrado%>%dplyr::count(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)

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

#experiência
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

#Nível de ensino por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar')%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P1_h ', 'Nivel de Ensino')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível de cargo por gênero
SoDfiltrado%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_g ', 'Nivel')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')

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

#Faixa salarial por gênero e atuacao
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`, `('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ ggplot2::facet_wrap(~`('P4_a ', 'Atuacao')`)

#Faixa salarial por gênero e nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`,`('P1_b ', 'Genero')`, `('P2_h ', 'Faixa salarial')`)%>%
  dplyr::group_by(`('P2_g ', 'Nivel')`, `('P1_b ', 'Genero')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P2_g ', 'Nivel')`, `('P1_b ', 'Genero')`)%>%
  dplyr::filter(!is.na(`('P2_h ', 'Faixa salarial')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_h ', 'Faixa salarial')`,
                               fill=`('P1_b ', 'Genero')`))+
  ggplot2::geom_col(position = 'dodge')+ ggplot2::facet_wrap(~`('P2_g ', 'Nivel')`)

#Satisfação por sexo
SoDfiltrado%>%
  dplyr::filter(!is.na(`('P2_k ', 'Você está satisfeito na sua empresa atual?')`))%>%
  dplyr::count(
    `('P1_b ', 'Genero')`,`('P2_k ', 'Você está satisfeito na sua empresa atual?')`)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
    dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_b ', 'Genero')`)

#Motivo da insatisfação por sexo
SoDfiltrado%>%
  dplyr::filter(`('P2_k ', 'Você está satisfeito na sua empresa atual?')`==0)%>%
  dplyr::group_by(`('P1_b ', 'Genero')`)%>%
  dplyr::summarise(oportunidade=sum(`('P2_l_a ', 'Falta de oportunidade de crescimento no emprego atual')`, na.rm = T)/dplyr::n(),
                   salario=sum(`('P2_l_b ', 'Salário atual não corresponde ao mercado')`, na.rm = T)/dplyr::n(),
                   lider=sum(`('P2_l_d ', 'Gostaria de trabalhar em em outra área de atuação')`, na.rm = T)/dplyr::n(),
                   area=sum(`('P2_l_d ', 'Gostaria de trabalhar em em outra área de atuação')`, na.rm = T)/dplyr::n(),
                   beneficios=sum(`('P2_l_e ', 'Gostaria de receber mais benefícios')`, na.rm = T)/dplyr::n(),
                   ambiente=sum(`('P2_l_f ', 'O clima de trabalho/ambiente não é bom')`, na.rm = T)/dplyr::n(),
                   maturidade=sum(`('P2_l_g ', 'Falta de maturidade analítica na empresa')`, na.rm = T)/dplyr::n()
                   )

"teste=SoDfiltrado%>%dplyr::filter(`('P1_b ', 'Genero')`=='Masculino' &
                                    `('P2_k ', 'Você está satisfeito na sua empresa atual?')`==0)
"

SoDfiltrado%>%
  dplyr::filter(`('P4_a ', 'Atuacao')`%in%c('Análise de Dados',
                                            'Ciência de Dados'))%>%
  dplyr::count(`('P1_b ', 'Genero')`, `('P4_a ', 'Atuacao')`)
#mulheres tem 2.75 analistas para cada cientista
#homens tem 1.9 analistas para cada cientista

