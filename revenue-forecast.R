library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(timetk)


# Load Data
dir = 'C:/Users/HP pc/Desktop/Masters/UoNA/3. Summer 2022/DATA522/Project/CustomerSubscription/'
cust_cases <- read.csv(paste0(dir,'customer_cases.csv'))
cust_info <- read.csv(paste0(dir,'customer_info.csv'))
cust_prod <- read.csv(paste0(dir,'customer_product.csv'))
prod_info <- read.csv(paste0(dir,'product_info.csv'))

cust_cases %>% group_by(channel, reason) %>% count()

cust_cases %>% head()
cust_info %>% head()
cust_prod %>% head()
prod_info %>% head()


# Clean Data
cust_cases <- cust_cases[-1]
cust_info <- cust_info[-1]
cust_prod <- cust_prod[-1]


cust_prod %>%
  #filter(!is.na(cancel_date_time))
  group_by(c_date = as.Date(cancel_date_time)) %>%
  count() %>% View


# Check for Duplicates
cust_cases %>% group_by(date_time, customer_id) %>% count() %>% filter(n > 1)
cust_info %>% group_by(customer_id) %>% count() %>% filter(n > 1)
cust_prod %>% group_by(customer_id, product) %>% count() %>% filter(n > 1)


# Add Columns
cust_cases$date <- as.Date(cust_cases$date_time)
cust_cases$year <- as.numeric(format(cust_cases$date, '%Y'))
cust_cases$month <- as.numeric(format(cust_cases$date, '%m'))
cust_cases <- cust_cases %>% select(-date_time)


cust_prod <- 
  cust_prod %>% 
  mutate(prod_desc = case_when(product == 'prd_1' ~ 'Annual',
                               product == 'prd_2' ~ 'Monthly'))

cust_prod$signup_date <- as.Date(cust_prod$signup_date_time)
cust_prod$signup_year <- as.numeric(format(cust_prod$signup_date, '%Y'))
cust_prod$signup_month <- as.numeric(format(cust_prod$signup_date, '%m'))

cust_prod$cancel_date <- as.Date(cust_prod$cancel_date_time)
cust_prod <- cust_prod %>% replace_na(list(cancel_date = as.Date('2022-01-01')))
cust_prod$cancel_year <- as.numeric(format(cust_prod$cancel_date, '%Y'))
cust_prod$cancel_month <- as.numeric(format(cust_prod$cancel_date, '%m'))

cust_prod$duration <- as.numeric(cust_prod$cancel_date - cust_prod$signup_date)
cust_prod <- cust_prod %>% select(-signup_date_time, -cancel_date_time)


### Data Analysis ###
 
# Count Plot

plot_dir = paste0(dir,'Plots/')

png(file = paste0(plot_dir, 'new_signups.png'), width = 800, height = 600)

as.data.frame(cust_cases) %>%
  filter(reason == 'signup') %>%
  ggplot(aes(fill = factor(year), x = factor(month))) +
  geom_bar(position = 'dodge') +
  ggtitle('New Signups') +
  labs(x = 'Month', y = 'Frequency', fill = 'Year')

dev.off()


cust_prod_info <- merge(cust_info, cust_prod, by = 'customer_id') 


png(file = paste0(plot_dir, 'user_age_dist.png'), width = 800, height = 600)

as.data.frame(cust_prod_info) %>%
  ggplot(aes(x = age)) +
  geom_histogram(position = 'dodge', binwidth = 0.5) +
  facet_grid(rows = vars(gender), cols = vars(prod_desc)) +
  labs(x = 'Age', y = 'Count')

dev.off()


png(file = paste0(plot_dir, 'subs_dur_dist.png'), width = 800, height = 600)

as.data.frame(cust_prod) %>%
  ggplot(aes(x = duration)) +
  geom_histogram(position = 'dodge', bins = 80) +
  geom_freqpoly(bins = 80) +
  facet_grid(cols = vars(prod_desc)) +
  labs(x = 'Duration', y = 'Count')

dev.off()


dupe_cases <- cust_cases %>% group_by(customer_id, year, month) %>% count() %>% filter(n > 1)

unique_cust_case_year_month <- cust_cases %>% distinct(customer_id, year, month, .keep_all = TRUE)
cust_sub = merge(unique_cust_case_year_month, cust_prod, by = 'customer_id')

signup_df <- cust_sub %>%
  filter(reason == 'signup') %>%
  group_by(year, month) %>%
  dplyr::summarise(signup = n())


cust_cancel_df <-
  cust_sub %>%
  filter(cancel_year != 2022) %>%
  group_by(cancel_year, cancel_month) %>%
  summarize(cust_cancel = n())


year = as.array(c(2017, 2018, 2019, 2020, 2021))
month = seq(1, 12)
total_sub = matrix(0, 60 ,4)
idx = 1


# y = 2017
# m = 1
# total_sub[1,1] = nrow(cust_sub %>% filter(signup_date < as.Date(paste(y+1,1,1, sep = '-'))&
#                       cancel_date >= as.Date(paste(y,m,1, sep = '-')) &
#                       signup_month == m &
#                       product == 'prd_1'))

#sample <- (cust_sub %>% filter(signup_date < as.Date(paste(y+1,1,1, sep = '-'))) %>% count() %>% as.list())[1]


for(y in year){
  for(m in month){
    total_sub[idx, 1] = y
    total_sub[idx, 2] = m
    
    if(m == 12){
      total_sub[idx, 3] = nrow(cust_sub %>% 
        filter(signup_date < as.Date(paste(y+1,1,1, sep = '-')) & 
                 cancel_date >= as.Date(paste(y,m,1, sep = '-')) & 
                 signup_month == m & 
                 product == 'prd_1'))
      total_sub[idx, 4] = nrow(cust_sub %>% 
        filter(signup_date < as.Date(paste(y+1,1,1, sep = '-')) &
                 cancel_date >= as.Date(paste(y,m,1, sep = '-')) &
                 product == 'prd_2'))
    }
    else{
      total_sub[idx, 3] = nrow(cust_sub %>% 
        filter(signup_date < as.Date(paste(y,m+1,1, sep = '-')) &
                 cancel_date >= as.Date(paste(y,m,1, sep = '-')) &
                 signup_month == m &
                 product == 'prd_1'))
      total_sub[idx, 4] = nrow(cust_sub %>% 
        filter(signup_date < as.Date(paste(y,m+1,1, sep = '-')) &
                 cancel_date >= as.Date(paste(y,m,1, sep = '-')) &
                 product == 'prd_2'))
    }
    idx = idx+1
  }
}

total_sub_df <-
  as.data.frame(total_sub) %>% rename(sub_year = V1, sub_month = V2, prod_1_sub = V3, prod_2_sub = V4)


unique_cust_cases <- cust_cases %>% distinct(customer_id, .keep_all = TRUE)
existing_cust <- nrow(cust_prod %>% filter(!(customer_id %in% c(cust_cases$customer_id))))
total_cust_count <- nrow(unique_cust_cases %>% filter(year == 2017 & month == 1)) + existing_cust

total_cust <- c()

for(y in year){
  for(m in month){
    if(y == 2017 & m == 1){
      total_cust <- append(total_cust, total_cust_count)
    }
    else{
      total_cust_count = total_cust_count + (signup_df %>% filter(year == y & month == m))$signup[1] -
                         (cust_cancel_df %>% filter(cancel_year == y & cancel_month == m))$cust_cancel[1]
      total_cust <- append(total_cust, total_cust_count)
    }
    
  }
}

cust_df <- merge(signup_df, cust_cancel_df, by.x = c('year', 'month'), by.y = c('cancel_year','cancel_month'))
cust_df <- merge(cust_df, total_sub_df, by.x = c('year', 'month'), by.y = c('sub_year','sub_month')) %>% arrange(year, month)

cust_df$total_cust = total_cust


signup_rate <- as.array(tail(cust_df$signup,-1)) / as.array(head(cust_df$total_cust, -1))
signup_rate <- append(0, signup_rate)
cust_df$signup_rate = as.numeric(as.list(signup_rate))

cust_cancel_rate <- as.array(tail(cust_df$cust_cancel,-1)) / as.array(head(cust_df$total_cust, -1))
cust_cancel_rate <- append(0, cust_cancel_rate)
cust_df$cust_cancel_rate = as.numeric(as.list(cust_cancel_rate))

cust_df$annual_sub_revenue_million <- cust_df$prod_1_sub*1200/1000000
cust_df$monthly_sub_revenue_million <- cust_df$prod_2_sub*125/1000000
cust_df$annual_sub_rate <- cust_df$prod_1_sub/cust_df$total_cust
cust_df$monthly_sub_rate <- cust_df$prod_2_sub/cust_df$total_cust

#cust_df <- cust_df %>% select(-cancel_year, -cancel_month, -sub_year, -sub_month)


png(file = paste0(plot_dir, 'new_signup_rate.png'), width = 800, height = 600)

ggplot(data = tail(cust_df, -1), aes(x = factor(month), y = signup_rate, group = factor(year), colour = factor(year))) +
  geom_line() +
  #geom_point() +
  scale_y_continuous(limits = c(0,0.015)) +
  labs(x = 'Month', y = 'Signup Rate', colour = 'Year')

dev.off()


png(file = paste0(plot_dir, 'cust_cancel_rate.png'), width = 800, height = 600)

ggplot(data = tail(cust_df, -1), aes(x = factor(month), y = cust_cancel_rate, group = factor(year), colour = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,0.015)) +
  labs(x = 'Month', y = 'Customer Cancel Rate', colour = 'Year')

dev.off()


png(file = paste0(plot_dir, 'subs.png'), width = 800, height = 600)

p1 <- 
  ggplot(cust_df, aes(x = factor(month), y = annual_sub_revenue_million, group = factor(year), colour = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,19)) +
  labs(x = 'Month', y = 'Annual Sub Revenue Million', colour = 'Year')
p2 <- 
  ggplot(cust_df, aes(x = factor(month), y = monthly_sub_revenue_million, group = factor(year), colour = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,19)) +
  labs(x = 'Month', y = 'Monthly Sub Revenue Million', colour = 'Year')
p3 <- 
  ggplot(cust_df, aes(x = factor(month), y = annual_sub_rate, group = factor(year), colour = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,0.45)) +
  labs(x = 'Month', y = 'Annual Sub Rate', colour = 'Year')
p4 <- 
  ggplot(cust_df, aes(x = factor(month), y = monthly_sub_rate, group = factor(year), colour = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,0.45)) +
  labs(x = 'Month', y = 'Monthly Sub Rate', colour = 'Year')

gridExtra::grid.arrange(p1,p2,p3,p4)
  
dev.off()


annual_sub <- as.Date(paste(cust_df$year, cust_df$month, '01', sep='-'))
annual_sub_df <- data.frame(month = annual_sub, annual_sub_revenue_million=cust_df$annual_sub_revenue_million)
rownames(annual_sub_df) <- annual_sub_df$month
annual_sub_df <- annual_sub_df %>% select(-month)

train_split <- c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,37, 38, 39, 40)
test_split <- c(41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)

train_data <- annual_sub_df[train_split, ]
test_data <- annual_sub_df[test_split, ]

train_acf_df <- with(acf(train_data$annual_sub_revenue_million, plot = FALSE), data.frame(lag, acf) )

ggplot(train_acf_df, aes(x = lag, y = acf)) +
  geom_line()






