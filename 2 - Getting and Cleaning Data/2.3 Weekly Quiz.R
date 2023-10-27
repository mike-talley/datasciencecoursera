# Week 3 Quiz
library(dplyr)
library(jpeg)

## 1

data <- read.csv("/Users/michaeltalley/Downloads/getdata_data_ss06hid (1).csv")

data <- data %>%
  mutate(flag = ifelse(ACR == 3 & AGS == 6, 1, 0)) %>%
  filter(flag == 1)

which(data$ACR == 3 & data$AGS == 6)


## 2
y <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
pic <- download.file(y,'y.jpg')

jpeg <- readJPEG("/Users/michaeltalley/Downloads/getdata_jeff.jpeg", native = TRUE)
quantile(jpeg, probs = c(0.3, 0.8))

## 3

gdp <- read.csv("/Users/michaeltalley/Downloads/getdata_data_GDP.csv")
gdp <- gdp[-c(1:4), ]
gdp <- gdp %>%
  filter(!is.na(as.numeric(Gross.domestic.product.2012)))
edu <- read.csv("/Users/michaeltalley/Downloads/getdata_data_EDSTATS_Country.csv")
# edu$X <- edu$CountryCode
edu$X.2 = edu$Long.Name

combo <- full_join(gdp, edu)
combo <- combo %>%
  # filter(CountryCode == X)
  arrange(desc(as.numeric(Gross.domestic.product.2012))) %>%
  mutate(X.3 = as.numeric(X.3))

summary <- combo %>%
  group_by(Income.Group) %>%
  summarise(high = mean(as.numeric(Gross.domestic.product.2012), na.rm = TRUE))

mean(as.numeric(combo$X.3)[combo$Income.Group == "High income: OECD"], na.rm = TRUE)
sum(combo$X.3, na.rm = TRUE) / length(combo$X.3[!is.na(combo$X.3)])


## 5

quant <- combo %>%
  arrange((as.numeric(Gross.domestic.product.2012))) %>%
  filter(as.numeric(Gross.domestic.product.2012) < 39)

answer <- count(quant["Income.Group" == "Lower middle income"])
length(quant[quant$Income.Group == "Lower middle income"])
