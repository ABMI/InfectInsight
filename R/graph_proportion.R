#' Drawing proportion graph
#'
#' @param Cohort_dg
#'
#' @export sexProportionGraph
sexProportionGraph <- function(){
  tempSex <- Cohort_dg %>% distinct(SUBJECT_ID, GENDER_SOURCE_VALUE)
  sexProportion <- plot_ly(tempSex, labels = ~GENDER_SOURCE_VALUE, type = 'pie',
                           textposition = 'inside',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF', size = 30),
                           showlegend = F,
                           marker = list(colors = c("#FF7F0E", "#1F77B4")))
  return(sexProportion)
}

#' @export ageProportionGraph
ageProportionGraph <- function(){
  tempAge <- Cohort_dg %>%
    distinct(SUBJECT_ID, GENDER_SOURCE_VALUE, ageGroup) %>%
    select(GENDER_SOURCE_VALUE, ageGroup) %>%
    group_by(GENDER_SOURCE_VALUE, ageGroup) %>%
    summarise(n=n())
  tempAge <- rename(tempAge, gender = GENDER_SOURCE_VALUE)
  tempAge <- tempAge %>% mutate(percentage = round(n/sum(n) * 100, digits = 2))

  temptext <- Cohort_dg %>% distinct(SUBJECT_ID, ageGroup) %>%
    select(ageGroup) %>%
    group_by(ageGroup) %>%
    summarise(n=n())
  temptext <- temptext %>% mutate(percentage = round(n/sum(n) * 100, digits = 2))
  temptext <- temptext %>% mutate(total = paste0(n, ' (', percentage, '%)'))


  ageProportion <- ggplot(tempAge, aes(x = ageGroup, y = n, fill = gender, text = paste0('ageGroup: ', ageGroup, '\n ', n, ' (', percentage, '%)'))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#FF7F0E", "#1F77B4"), name = 'Gender') +
    labs(x = '', y = 'Number of People') +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_classic() +
    theme(legend.position = 'bottom')

  ageProportion <- ggplotly(ageProportion) %>%
    layout(legend = list(orientation = "h",  x = 0.25))

  return(ageProportion)
}



