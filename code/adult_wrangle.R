pkgs <- c('dplyr','data.table','stringr',
          'ggplot2','rvest')
sapply(pkgs,require,character.only = TRUE)

#train <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",data.table = FALSE) 
#test <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",skip = 1,data.table = FALSE)
#adult <- rbind(train,test) 
#remove(train,test)
#write.csv(adult,'D:/github_desktop/adult_census/adult_census/adult.csv',row.names = FALSE)

adult <- read.csv('D:/github_desktop/adult_census/adult_census/data/adult.csv')

glimpse(adult)
colnames(adult) <- c("age","workclass","fnlwgt","education",
                     "education_num","martial_status","occupation",
                     "relationship","race","sex","capital_gain",
                     "capital_loss","hours_per_week","native_country",
                     "income_condition")

# change "?" into NA.
adult[,c(2,7,14)] <- apply(adult[,c(2,7,14)],2,function(x)
  str_replace_all(x,'[?]','missing'))

adult[,c(c(2,4,6:10,14,15))] <- apply(adult[,c(2,4,6:10,14,15)],2,
                     function(x)str_replace_all(x,'[:space:]',''))
         
adult[,15] <- ifelse(str_detect(adult[,15],'>'),'over_50k','under_50k') %>%
  factor(.,levels = c('under_50k','over_50k'))
unique(adult[,15])

# age ####
adult$age2 <- ifelse(adult$age %in% 16:24,"age_a",
              ifelse(adult$age %in% 25:34,"age_b",
              ifelse(adult$age %in% 35:44,"age_c",
              ifelse(adult$age %in% 45:54,"age_d",
              ifelse(adult$age %in% 55:64,"age_e",
              ifelse(adult$age %in% 65:90,"age_d","error"))))))
adult$age2 <- factor(adult$age2)

# age2 visualization ####
ggplot(adult,aes(age2,group = factor(income_condition),
                      fill = factor(income_condition))) +
  geom_bar(position = 'fill') +
  xlab('Age_group') + ylab('Frequency') +
  scale_fill_discrete(name = 'Income')
  
# workclass ####
# visualize by default group
ggplot(adult,aes(workclass,group = income_condition,
                           fill  = income_condition)) +
  geom_bar(position = 'fill') +
  scale_fill_discrete(name = 'income')

# group by four
adult$work <- ifelse(adult$workclass == "Private","Private",
              ifelse(endsWith(adult$workclass,'gov'),'gov',
              ifelse(endsWith(adult$workclass,'inc'),'inc',
              ifelse(adult$workclass == 'missing','missing','others')))) 
adult$work <- factor(adult$work)

# vis
ggplot(adult,aes(work,group = income_condition,
                       fill = income_condition)) + geom_bar(position = 'fill')

# private or not
adult$if_private <- ifelse(adult$workclass == 'Private','Private','not_Private')
adult$if_private <- factor(adult$if_private)

# education to korean version
adult$educated <- ifelse(adult$education_num %in% c(1:6),'elementary',
                  ifelse(adult$education_num %in% c(7:9),'middle',
                  ifelse(adult$education_num %in% c(10:12),'high',
                  ifelse(adult$education_num %in% c(13:16),'college','error'))))
adult$educated <- factor(adult$educated,levels = c('elementary','middle',
                                                   'high','college'))

# visualize new education
ggplot(adult,aes(educated,group = income_condition,
                          fill = income_condition)) +
  geom_bar(position = 'fill')

# martial status.
ggplot(adult,(aes(martial_status,group = income_condition,
                                 fill = income_condition))) +
  geom_bar(position = 'fill')

# with three group. ####
adult$spouse <- ifelse(endsWith(adult$martial_status,'spouse'),
                       'with_spouse','without_spouse')
adult$spouse <- factor(adult$spouse)

# occupation ####
tem <- adult %>% group_by(occupation,income_condition) %>%
  summarise(aa = n()) 
tem2 <- merge(tem %>% filter(income_condition == "over_50k"),
              tem %>% filter(income_condition == "under_50k"),
              by = c("occupation" = "occupation"))
tem2$portion <- sort(tem2$aa.x / (tem2$aa.x + tem2$aa.y))
tem2$group <- rep(c('low_job','mid_job','hig_job'),each = 5)
tem2 <- tem2[,c(1,7)]
adult <- left_join(adult,tem2,by = 'occupation')
adult$work <- factor(adult$group)
remove(tem,tem2)

# relationship ####
adult$live_with <- ifelse(adult$relationship %in% c("Husband","Wife"),"Two",
                    ifelse(adult$relationship == "Own-child","Own_child","Alone"))
adult$live_with <- factor(adult$live_with, levels = c("Alone","Two","Own_child"))

# race ####
adult$if_white <- ifelse(adult$race == "White","White","Not_white")
adult$if_white <- factor(adult$if_white,levels = c("Not_white","White"))

# sex는 그대로. ####
adult$sex <- factor(adult$sex, levels = c("Male","Female"))

# capital gain
# 1. 0인가 0이 아닌가 - isgain ####
adult$isgain <- ifelse(adult$capital_gain == 0,"No_gain","gain")
adult$isgain <- factor(adult$isgain, levels = c("No_gain","gain"))

# 2. 0 ~ 100만 ~ 1000만 ~ - gain_three ####
adult$gain_three <- ifelse(adult$capital_gain == 0,"No gain",
                     ifelse(between(adult$capital_gain,1,10000),
                            "Medium gain","Higer gain"))

adult$gain_three <- factor(adult$gain_three, 
                      levels = c("No gain","Medium gain","Higer gain"))

# capital_loss
adult$isloss <- ifelse(adult$capital_loss == 0,"No_loss","loss")
adult$isloss <- factor(adult$isloss, levels = c("No_loss","loss"))

# hours per week. ####
# OECD labor force statistics. 38.5
# median?
adult$labor_oecd <- ifelse(adult$hours_per_week < 38.5,"lower","higher")
adult$labor_oecd <- factor(adult$labor_oecd, levels = c("lower","higher"))

# native_country
# 1. usa or not
adult$isusa <- ifelse(adult$native_country == "United-States","USA","Not_USA")
adult$isusa <- factor(adult$isusa,levels = c("Not_USA","USA"))
table(adult$isusa)

# 2. group by continent
# continent data load.
adult$native_country <- str_replace_all(adult$native_country,'-',' ')
url <- 'https://country-code.cl/'
a <- read_html(url) %>% html_node('.tablesorter.mark') %>% html_table()
idx <- t(sapply(a$Name,function(x)str_locate(x,'[:upper:]')))
idx[,1] <- row.names(idx)
row.names(idx) <- 1:nrow(idx)
a[,3] <- str_sub(idx[,1],idx[,2],sapply(idx[,1],str_length))
a[,1] <- str_replace_na(a[,1])
a <- a[,c(1,3)]
a[,2] <- gsub("(.*),.*", "\\1", a[,2])

test <- left_join(adult,a,by = c('native_country' = 'Name'))

# unmatch data.
tem <- data.frame(Continent = c('MS','AS','EU','SA','AS',
                                'EU','OC','EU','NA','AS','EU'),
                  Name = unique(test$native_country[is.na(test$Continent)]))
a <- rbind(a,tem)
adult <- left_join(adult,a,by = c('native_country' = 'Name'))
adult$Continent <- factor(adult$Continent)
remove(a,idx,tem,test)

# write Rdata
adult <- adult[,c(15,16,10,17:29)]
save(adult,file = 'D:/github_desktop/adult_census/adult_census/data/adult.RData')
