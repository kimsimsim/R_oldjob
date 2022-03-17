library(dplyr)
library(ggplot2)
library(foreign)
oldjob <- read.csv("경기도 노인일자리현황.csv")
str(oldjob)
oldjob <- oldjob %>%
  select(기준년도, 기준월, 시군명, 성별, 노인수, 일자리수) %>% 
  rename(year = 기준년도,
         month = 기준월,
         region = 시군명,
         sex = 성별,
         old_num = 노인수,
         job_num = 일자리수)
summary(oldjob)

table(oldjob$region)

oldjob_num_1 <- oldjob %>% 
  filter(sex == "계") %>%
  group_by(region) %>% 
  mutate(pct1 = round(old_num/1818359*100,1)) %>% 
  select(region, old_num, pct1) %>% 
  arrange(-pct1)

oldjob %>% 
  filter(sex == "계") %>% 
  summarise(total = sum(job_num))

oldjob_num_2 <- oldjob %>% 
  filter(sex == "계") %>%
  group_by(region) %>% 
  mutate(pct2 = round(job_num/91832*100,1)) %>% 
  select(region, job_num, pct2) %>% 
  arrange(-pct2)

oldjob_num_2
oldjob_num_1

oldjob_1 <- oldjob %>% 
  filter(sex %in% c("남", "여"))
oldjob_1

oldjob_num <- left_join(oldjob_num_1, oldjob_num_2, by = "region")
oldjob_num

ggplot(oldjob_num, aes(x=region, y=pct1, pct2)) + geom_col() + coord_flip()

var.test(data = oldjob_1, job_num~sex)
t.test(data = oldjob_1, job_num~sex, var.equal = F)

2137.7097 - 824.6129

oldjob %>% 
  filter(sex == "남") %>% 
  summarise(total1 = sum(old_num), total2 = sum(job_num))

oldjob %>% 
  filter(sex %in% c("남", "여")) %>% 
  group_by(sex) %>% 
  summarise(total_old = sum(job_num), total_job = sum(job_num)) %>% 
  mutate(pct = round(total_job/total_old*100,1))

oldjob %>% 
  filter(sex %in% c("남", "여")) %>% 
  group_by(region) %>% 
  mutate(total = sum(job_num),
         pct = round(job_num/total*100,1))

RA<-lm(data = oldjob, job_num~old_num)
summary(RA)


3.501e-02
