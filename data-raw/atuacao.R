#Nível de ensino por Nível de cargo
SoDfiltrado%>%
  dplyr::count(`('P2_g ', 'Nivel')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
      !is.na(`('P2_g ', 'Nivel')`))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P2_g ', 'Nivel')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível de ensino por atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`, `('P1_h ', 'Nivel de Ensino')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`)%>%
  dplyr::filter(`('P1_h ', 'Nivel de Ensino')`!='Prefiro não informar' &
                  `('P4_a ', 'Atuacao')`%notin%c('Outra',
                                                 'Buscando emprego na área de dados.'))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P1_h ', 'Nivel de Ensino')`))+
  ggplot2::geom_col(position = 'dodge')

#Nível de cargo por atuação
SoDfiltrado%>%
  dplyr::count(`('P4_a ', 'Atuacao')`, `('P2_g ', 'Nivel')`)%>%
  dplyr::group_by(`('P4_a ', 'Atuacao')`)%>%
  dplyr::mutate(perc = n/sum(n)*100)%>%
  dplyr::arrange(`('P4_a ', 'Atuacao')`)%>%
  dplyr::filter(!is.na(`('P2_g ', 'Nivel')`) &
                  `('P4_a ', 'Atuacao')`%notin%c('Outra',
                                                 'Buscando emprego na área de dados.'))%>%
  ggplot2::ggplot(ggplot2::aes(x=perc, y=`('P4_a ', 'Atuacao')`,
                               fill=`('P2_g ', 'Nivel')`))+
  ggplot2::geom_col(position = 'dodge')




