pkgs <- c('dplyr','data.table','stringr','ggplot2')
sapply(pkgs,require,character.only = TRUE)

setwd("D:/승훈/Data/R 스터디/2018 미니프로젝트/data")
train <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",data.table = FALSE) 
test <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",skip = 1,data.table = FALSE)

adult <- rbind(train,test) 
remove(train,test)

glimpse(adult)
colnames(adult) <- c("age","workclass","fnlwgt","education",
                     "education_num","martial_status","occupation",
                     "relationship","race","sex","capital_gain",
                     "capital_loss","hours_per_week","native_country",
                     "income_condition")

adult[,c(c(2,4,6:10,14,15))] <- apply(adult[,c(2,4,6:10,14,15)],2,
                                      function(x)str_replace_all(x,'[:space:]',''))
adult[,15] <- str_replace_all(adult[,15],'[.]','')
unique(adult[,15])

# change "?" into NA.
adult[,c(2,7,14)] <- apply(adult[,c(2,7,14)],2,function(x)
                        str_replace_all(x,'[?]','missing'))

# age ####
adult$age2 <- ifelse(adult$age %in% 16:24,"age_a",
              ifelse(adult$age %in% 25:34,"age_b",
              ifelse(adult$age %in% 35:44,"age_c",
              ifelse(adult$age %in% 45:54,"age_d",
              ifelse(adult$age %in% 55:64,"age_e",
              ifelse(adult$age %in% 65:90,"age_d","error"))))))
adult$age2 <- factor(adult$age2)

# age2 visualization ####
ggplot(adult,aes(factor(age2),group = factor(income_condition),
                              fill = factor(income_condition))) + geom_bar() +
  xlab('Age_group') + ylab('Frequency') +
  scale_fill_discrete(name = 'Income',
                    labels = c('under_50k','over_50k'))

# workclass ####
# grouping
# private or not
adult$if_private <- ifelse(adult$workclass == "Private","Private",
                    ifelse(endsWith(adult$workclass,''))) 
table(endsWith(adult$workclass,'gov'))

table(adult$if_private)

# three group visualization ####

# education
# making dictionary
edu_num <- data.frame(table(adult$education_num))
edu_txt <- data.frame(table(adult$education))
edu_dic <- left_join(edu_txt,edu_num,by = c("Freq" = "Freq")) %>%
  arrange(Var1.y)

adult$education <- factor(adult$education, levels = edu_dic$Var1.x)
adult$edu_group <- ifelse(adult$education %in% edu_dic$Var1.x[1:8],"School_uncomplete",
                   ifelse(adult$education %in% edu_dic$Var1.x[9],"HS-grad",
                   ifelse(adult$education %in% edu_dic$Var1.x[10],"Some-college",
                   ifelse(adult$education %in% edu_dic$Var1.x[11:13],"college graduated",
                   ifelse(adult$education %in% edu_dic$Var1.x[14:16],"post college","error")))))

adult$edu_group <- factor(adult$edu_group,
                          levels = c("School_uncomplete", "HS-grad", "Some-college",
                                     "college graduated", "post college"))
remove(edu_num, edu_txt)

# income_condition group by new education group ####

# martial status.
# 배우자와 같이 사는가 살지 않는가? ####

tem <- data.frame(table(adult$martial_status))

# with two group. ####
adult$spouse <- ifelse(adult$martial_status %in% tem[2:4,1],
                     "Have_spouse","No_spouse")
adult$spouse <- factor(adult$spouse,levels = c("No_spouse","Have_spouse"))

# occupation ####
tem <- adult %>% group_by(occupation,income_condition) %>%
  summarise(aa = n()) 
tem2 <- merge(tem %>% filter(income_condition == ">50K"),
              tem %>% filter(income_condition == "<=50K"),
              by = c("occupation" = "occupation"))
tem2$x.por <- tem2$aa.x / (tem2$aa.x + tem2$aa.y)
tem2$y.por <- tem2$aa.y / (tem2$aa.x + tem2$aa.y)
tem2 <- tem2[order(tem2$x.por),]
adult$job_group <- ifelse(adult$occupation %in% tem2$occupation[c(1:3,5:8)],"lower",
                          ifelse(adult$occupation %in% tem2$occupation[c(9:15)],"medium",
                                 ifelse(is.na(adult$occupation),"upper","error")))
adult$job_group <- factor(adult$job_group,levels = c("lower","medium","upper"))

# relationship ####
tem <- data.frame(table(adult$relationship))

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

adult$labor_medi <- ifelse(adult$hours_per_week == 40,"median",
                    ifelse(between(adult$hours_per_week,1,39),"lower",
                    ifelse(adult$hours_per_week > 40,"higher","error")))
adult$labor_medi <- factor(adult$labor_medi,levels = c("lower","median","higher"))

# native_country
# 1. usa or not
tem <- data.frame(table(adult$native_country))
colnames(tem) <- c("country","freq")
tem$country <- as.character(tem$country)
adult$isusa <- ifelse(adult$native_country == "United-States","USA","Not_USA")
adult$isusa <- factor(adult$isusa,levels = c("Not_USA","USA"))
table(adult$isusa)

# 2. group by continent
# continent data load.
continent_info <- read.csv("D:/승훈/Data/Olympic analysis/ddd/country_conti.csv",
                           header = FALSE, stringsAsFactors = FALSE)
continent_info <- continent_info[,c(2,5)]
colnames(continent_info) <- c("Region","Country")

continent_info <- separate(continent_info,Region,into = c("region"),sep = " & ")
table(tem$country %in% continent_info$Country)
con_join <- left_join(tem,continent_info, by = c("country" = "Country"))

table(is.na(con_join$region))

# fill na.
con_join$region[is.na(con_join$region)] <- 
    c("Latin America","Latin America","Latin America","Europe","Europe",
      "Asia","Asia","Asia","Latin America","Latin America","Europe",
      "Asia","Asia","Latin America","North America","Europe")

con_join$region <- ifelse(endsWith(con_join$region,"Asia"),
                          "Asia",con_join$region)

adult <- left_join(adult,con_join, by = c("native_country" = "country"))
adult <- adult[,!(names(adult) %in% c("freq","Freq"))]
adult$region <- factor(adult$region)

# income_condition to factor
adult$income_condition <- factor(adult$income_condition, levels = c("<=50K",">50K"))
