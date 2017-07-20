# WranglingExercise1
Springboard Wrangling exercise 1
#load package
tbl_df(refine_original)
library(dplyr)
library(tidyr)

#clean company name -***don't know why these code doesn't work***
refine_original2 <- refine_original %>% mutate(company = ifelse(grepl("^phi|^Phi|^phl|^Phl|^fil", company, ignore.case = TRUE), "philips", company)) %>% 
 mutate(company = ifelse(grepl("^ak|^Ak^AK|", company, ignore.case = TRUE), "akzo", company)) %>% 
mutate(company= ifelse(grepl("^van|^Van",company,ignore.case = TRUE ),"van houton", company )) %>% 
mutate(company= ifelse(grepl("^uni|^Uni",company,ignore.case = TRUE ),"unilever", company ))
#clean company philips name
refine_original2 <- refine_original %>% mutate(company = ifelse(grepl("^phi|^Phi|^phl|^Phl|^fil", company, ignore.case = TRUE), "philips", company))

#clean akzo name
refine_original4 <- refine_original2 %>% mutate(company = ifelse(grepl("^ak|^Ak|^AK", company, ignore.case = TRUE), "akzo", company))
#clean van houton name
refine_original5 <- refine_original4 %>% mutate(company = ifelse(grepl("^van|^Van", company, ignore.case = TRUE), "van houten", company))
#clean unilever name
refine_original6 <- refine_original5 %>% mutate(company = ifelse(grepl("^Uni|^uni", company, ignore.case = TRUE), "unilever", company))
#combine address, city, and country variable to "address"
refine_original7 <- unite(refine_original6, "address",address, city, country, sep = "," )
#separate product code and number:
refine_original8<- separate(refine_original7, `Product code / number`, c("product code", "number"))

#add product category
refine_original13 <- refine_original8 %>% mutate("product_category" = ifelse(`product code` == "p", "Smartphone", "")) %>%
  mutate("product_category" = ifelse(`product code` == "x", "Laptop", product_category)) %>%
  mutate("product_category" = ifelse(`product code` == "v", "TV", product_category)) %>%
  mutate("product_category" = ifelse(`product code` == "q", "Tablet", product_category))
#Create dummy variables for company
refince_original14 <- refine_original13 %>% mutate("company_philips"=ifelse(company == "philips", 1, 0)) %>% 
mutate("company_akzo"=ifelse(company == "akzo", 1, 0)) %>% 
mutate("company_van_houten"=ifelse(company == "van houten", 1, 0)) %>% 
mutate("company_unilever"=ifelse(company == "unilever", 1, 0))

#Create dummy variables for product category
refine_clean <- refince_original14 %>% mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
mutate(product_TV = ifelse(product_category == "TV", 1, 0)) %>%
mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))
