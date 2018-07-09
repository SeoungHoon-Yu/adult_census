library(dplyr)
library(ggplot2)

names(adult)
age_group <- c('16:24','25:34','35:44','45:54','55:64','over_65')
ggplot(adult,aes(age2,
       fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_x_discrete(labels = age_group) +
  scale_fill_manual(values=c('#999999','#EEAD0E')) +
  xlab('age_group') + ylab('Frequency') +
  theme(legend.position = 'none')

ggplot(adult,aes(sex,
       fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=c('#999999','#EEAD0E')) +
  xlab('Sex') + ylab('Frequency') +
  theme(legend.position = 'none')

ggplot(adult,aes(if_private,
       fill = income_condition)) +
  geom_bar(position = 'identity') +
  scale_fill_manual(values=c('#999999','#56B4E9')) +
  xlab('if_private') + ylab('Frequency') +
  theme(legend.position = 'none')

ggplot(adult,aes(educated,
       fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=c('#999999','#56B4E9')) +
  xlab('education') + ylab('Frequency') +
  theme(legend.position = 'none')

ggplot(adult,aes(live_with,
       fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Dark2') +
  ylab('Frequency') + theme(legend.position = 'null')

ggplot(adult,aes(if_white,
       fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Dark2') +
  ylab('Frequency') + theme(legend.position = 'null')

ggplot(adult,aes(labor_oecd,
       fill = income_condition)) +
  geom_bar(position = 'identity') +
  scale_fill_brewer(palette = 'Dark2') +
  ylab('Frequency') + theme(legend.position = 'null')

ggplot(adult,aes(isusa,
                 fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Dark2') +
  ylab('Frequency') + theme(legend.position = 'null')

ggplot(adult,aes(Continent,
                 fill = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Dark2') +
  scale_x_discrete(labels = c('Asia','Europe','Missing',
                              'N_America','Oceania','S_America')) +
  ylab('Frequency') + theme(legend.position = 'null')
