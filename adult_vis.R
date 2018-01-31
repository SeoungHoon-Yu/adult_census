# adult data visualization.

# age
adult %>% ggplot(aes(income_condition,age)) +
  geom_jitter(aes(col = income_condition),alpha = 0.09) 

# age2 visualization
adult %>% group_by(age2,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(income_condition, aa ,fill = age2)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#7EC0EE", "#436EEE", "#27408B"),
                    labels = c("old\n(1781/551)","work_age\n(16642/7176)",
                               "young\n(6297/114)")) +
  theme(legend.key.height = unit(1,"cm"))  

# workclass
adult %>% group_by(workclass) %>% summarise(aa = n()) %>% arrange(aa) %>%
  ggplot(aes(workclass,aa)) + geom_bar(stat = "identity")

# three group visualization.
adult %>% group_by(work_group) %>% summarise(aa = n()) %>% arrange(aa) %>%
  ggplot(aes(work_group,aa)) + geom_bar(stat = "identity")

# income_condition group by new education group.
adult %>% group_by(edu_group,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(edu_group,aa,group = edu_group,fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# martial status.
# 배우자와 같이 사는가 살지 않는가?
adult %>% group_by(martial_status,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(martial_status,aa,group = martial_status,fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# with two group.
adult %>% group_by(spouse,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(spouse,aa, group = spouse, fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# occupation
# 일단 패스
adult %>% group_by(occupation,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(occupation,aa,group = occupation,fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# relationship
adult %>% group_by(relationship,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(relationship,aa,group = relationship, fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# race - if_white
adult %>% group_by(if_white,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(if_white,aa, fill = income_condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

adult %>% group_by(if_white,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(if_white,aa, fill = income_condition, group = if_white)) +
  geom_bar(stat = "identity", position =  "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# sex
adult %>% group_by(sex,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(sex,aa, fill = income_condition, group = sex)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# capital gain - 1.is gain
adult %>% group_by(isgain,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(isgain,aa, fill = income_condition, group = isgain)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# capital gain - 2. gain three
adult %>% group_by(gain_three,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(gain_three,aa, fill = income_condition, group = gain_three)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# capital loss - isloss
adult %>% group_by(isloss,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(isloss,aa, fill = income_condition, group = isloss)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# hours per week
adult %>% group_by(labor_oecd,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(labor_oecd,aa, fill = income_condition, group = labor_oecd)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))

# job_group
adult %>% group_by(job_group,income_condition) %>% summarise(aa = n()) %>%
  ggplot(aes(job_group,aa, group = job_group, fill = income_condition)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#575757", "#ABABAB"))


