#### 1. General House-keeping ####
# Clear environment.
rm(list=ls())

#### 2. Load libraries ####
# Load libraries to be used in data analysis.
pacman::p_load(pacman, tidyverse, lubridate, zoo, readxl, xts, dygraphs, scales)

#### 3. Import data ####
# Get GitHub url from Team 10's GitHub repository and paste below. Note: If url doesn't work you may need to update the url by copying and pasting a new url.
noise_url = "https://github.gatech.edu/raw/MGT-6203-Spring-2023-Canvas/Team-10/main/Data/grouped_311_noise%20v02.csv?token=GHSAT0AAAAAAAACUQFTZ676D32XC52OKJ7SZA2FC6A"
income_url = "https://github.gatech.edu/raw/MGT-6203-Spring-2023-Canvas/Team-10/main/Data/irs_income_zip.csv?token=GHSAT0AAAAAAAACUQFS647EPT2E44NNURZOZA2FHCA"
pop_url = "https://github.gatech.edu/raw/MGT-6203-Spring-2023-Canvas/Team-10/main/Data/population_zip_year.csv?token=GHSAT0AAAAAAAACUQFTM64SYZGAWOKFVU4MZA2FIWA"
rent_url = "https://github.gatech.edu/MGT-6203-Spring-2023-Canvas/Team-10/blob/main/Data/zillow_rent_value_index_zip_month.csv"
sales_url = "https://github.gatech.edu/raw/MGT-6203-Spring-2023-Canvas/Team-10/main/Data/zillow_sales_value_index_zip_month.csv?token=GHSAT0AAAAAAAACUQFS2J2O6XTJVDLLRGR4ZA2FW4Q"

# Import data from Team 10's GitHub repository.
noise_data <- read_csv(url(noise_url))
income_data <- read_csv(url(income_url))
pop_data <- read_csv(url(pop_url))
rent_data <- read_csv(url(rent_url))
sales_data <- read_csv(url(sales_url))

# Import zipped data that I saved locally (i.e., joined sales and rent data, building.xlsx).
# Note: If you plan on working with these data sets locally, you'll need to set your
# working directory for the session.
s_data <- read_csv("join_sales_index_datasets.csv")
r_data <- read_csv("join_rent_index_datasets.csv")
building <- read_excel("building_zip.xlsx")

#### 4. Exploratory Data Analysis & Data Preparation ####
# Inspect the column specifications for the rent and sales data.
spec(r_data)
spec(s_data)

# Inspect the structure of the entire dataframe without truncating the output. This
# code uses the list.len to specify the length of the output as the number of columns
# in the dataframe.
str(r_data, list.len = ncol(r_data))

# Identify top 5 and bottom 5 values for various variables
r_data %>% slice_max(qty_complaints, n = 10)
r_data %>% slice_min(qty_complaints, n = 10)

# Use dplyr to update variables to appropriate data type (e.g., factor, date, etc.)
# building with no zip codes data.
building <- building %>% 
  rename(building_zip = 'Zip Code') %>% 
  mutate(building_zip = factor(building_zip))

# zillow NYC housing rent data.
r_data <- r_data %>% 
  mutate_at(c("borough", "income_group"), factor) %>% 
  mutate_at(vars(matches('zip')), factor) %>%
  mutate(zillow_month = as.yearmon(zillow_month)) %>% 
  arrange(zillow_month) %>%
  mutate(zillow_month_day = as.Date(zillow_month_day)) %>% 
  filter(!zillow_zip %in% building$building_zip)
  
s_data <- s_data %>% 
  mutate_at(c("borough", "income_group"), factor) %>% 
  mutate_at(vars(matches('zip')), factor) %>%
  mutate(zillow_month = as.yearmon(zillow_month)) %>% 
  arrange(zillow_month) %>%
  mutate(zillow_month_day = as.Date(zillow_month_day)) %>% 
  filter(!zillow_zip %in% building$building_zip)


# Identify and count all NA values in each column vector in the "r_data" dataframe.
# Then create a new dataframe with that information and then filter to observe
# which variables have NA values.
r_data_na <- data.frame(colSums(is.na(r_data))) 
r_data_na <- r_data_na %>% 
  rename("na_count" = colSums.is.na.r_data..) %>% 
  filter(na_count > 0)
r_data_na

s_data_na <- data.frame(colSums(is.na(s_data))) 
s_data_na <- s_data_na %>% 
  rename("na_count" = colSums.is.na.s_data..) %>% 
  filter(na_count > 0)
s_data_na

# Test out removing all rows with NA values. Note: we need to investigate the NA
# values to better understand why there are NA values and whether it is appropriate 
# to exclude them.
r_data <- na.omit(r_data)
s_data <- na.omit(s_data)

# Inspect new dataframe and confirm that NA values have been removed.
colSums(is.na(r_data))
colSums(is.na(s_data))

# Summarize data (i.e., rental price, quantity of complaints, etc.) by month 
# and year.
r_data_noise_sum <- r_data %>% 
  group_by(zillow_month_day) %>% 
  summarize(sum_qty_comps = sum(qty_complaints)/1000)
r_data_noise_sum

s_data_noise_sum <- s_data %>% 
  group_by(zillow_month_day) %>% 
  summarize(sum_qty_comps = sum(qty_complaints)/1000)
s_data_noise_sum

r_data_rent_avg <- r_data %>% 
  group_by(zillow_month_day) %>% 
  summarize(rent_avg = mean(zillow_rent_value_index))
r_data_rent_avg

s_data_sales_avg <- s_data %>% 
  group_by(zillow_month_day) %>% 
  summarize(sales_avg = mean(zillow_sales_value_index)/1000)
s_data_sales_avg

# Convert data to time series object and order by date. 
r_data_ts <- xts(r_data_noise_sum[,-1, drop = FALSE], order.by = r_data_noise_sum$zillow_month_day)
r_data_ts

s_data_ts <- xts(s_data_noise_sum[,-1, drop = FALSE], order.by = s_data_noise_sum$zillow_month_day)
s_data_ts

r_data_ts2 <- xts(r_data_rent_avg[,-1, drop = FALSE], order.by = r_data_noise_sum$zillow_month_day)
r_data_ts2

s_data_ts2 <- xts(s_data_sales_avg[,-1, drop = FALSE], order.by = s_data_noise_sum$zillow_month_day)
s_data_ts2

# Visualize the noise complaint time series data using dygraphs package.
plot1 <- dygraph(r_data_ts) %>% 
  dyAxis("y", label = "Noise Complaints (Thousand)")
plot1

plot2 <- dygraph(s_data_ts) %>% 
  dyAxis("y", label = "Noise Complaints (Thousand)")
plot2

plot3 <- dygraph(r_data_ts2) %>% 
  dyAxis("y", label = "Average Rent Index Value (Dollars)")
plot3

plot4 <- dygraph(s_data_ts2) %>% 
  dyAxis("y", label = "Average Sales Index Value (Thousand Dollars)")
plot4

# Alternate time series plot using ggplot2.
plot5 <- ggplot(data = r_data_noise_sum, mapping = aes(x= zillow_month_day, y = sum_qty_comps)) +
  geom_line() +
  xlab("") +
  scale_x_date(date_labels = "%b %Y") +
  labs(y = "Monthly Complaints")
plot5


#### 5. Visualization ####
# Visualize data
ggplot(data = r_data, mapping = aes(x = zillow_month_day, y = comp_ratio_1000_total)) +
  geom_line()

ggplot(data = r_data, mapping = aes(x = borough, y = qty_complaints)) +
  geom_bar(stat = "identity") +
  coord_flip()


ggplot(data = r_data, mapping = aes(x = borough, y = qty_complaints)) +
  geom_boxplot()

ggplot(data = r_data, mapping = aes(x = borough, y = comp_ratio_1000_total)) +
  geom_boxplot()

ggplot(data = r_data, mapping = aes(x = zillow_zip, y = qty_complaints)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(data = r_data, mapping = aes(x = year, y = qty_complaints)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(data = r_data, mapping = aes(x = month, y = qty_complaints)) +
  geom_point(alpha = 0.2)
  

ggplot(data = r_data, mapping = aes(x = Total_income_per_capita, y = comp_ratio_1000_total)) +
  geom_point(alpha = 0.2)
  geom_smooth(method = lm, se = TRUE)

#### 6. Modeling ####
# Experiment with building linear regression models.
model1 <- lm(zillow_rent_value_index ~ zillow_zip + income_group, data = r_data)
summary(model1)

model2 <- lm(zillow_rent_value_index ~ zillow_zip + income_group + comp_ratio_1000_total, data = r_data)
summary(model2)

model3 <- lm(zillow_rent_value_index ~ comp_ratio_1000_total, data = r_data)
summary(model3)


