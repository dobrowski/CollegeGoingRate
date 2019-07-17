

### Libraries -----

library(tidyverse)
library(here)
library(ggthemes)
library(RColorBrewer)


cgr <- read_tsv( "cgr12mo18.txt", col_types = "ffccccccffffnnnnnnnnnnnn")


### College Type -----

cgr.mry <- cgr %>%
    filter(str_detect(CountyName, "Monterey"),
           ReportingCategory == "TA",
           CompleterType == "TA",
           CharterSchool == "All",
           AlternativeSchoolAccountabilityStatus == "All",
           !is.na(`High School Completers`))  %>%
    mutate(PercentUC = `Enrolled UC (12 Months)`/`High School Completers`,
           PercentCSU = `Enrolled CSU (12 Months)`/`High School Completers`,
           PercentCC = `Enrolled CCC (12 Months)`/`High School Completers`,
           PercentPrivate = `Enrolled In-State Private (2 and 4 Year) (12 Months)`/`High School Completers`,
           PercentOutCA = `Enrolled Out-of-State (12 Months)`/`High School Completers`,
           PercentTotal = PercentUC+PercentCSU+PercentCC+PercentPrivate+PercentOutCA,
           name = paste0(DistrictName," - ",SchoolName)) %>% 
    mutate(name = fct_reorder(name, PercentTotal))

ggplot(cgr.mry) +
    geom_col( aes(x = name, y = PercentTotal )) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_hc()

cgr.stack <- cgr.mry %>%
    select(name, starts_with("Percent")) %>%
    select(-PercentTotal) %>% 
    gather(key = "where","value", -name)


ggplot(cgr.stack) +
    geom_bar( aes(x =name , fill = where, weight = value )) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_hc() +
    scale_fill_brewer(palette="Dark2")

ggsave("All by College Type.png", width = 14, height = 10, dpi = 500)

#### Graph by Gender -------

cgr.gender <- cgr %>%
    filter(str_detect(CountyName, "Monterey"),
           AggregateLevel  %in% c("C","S"),  # School
           ReportingCategory %in% c("GM","GF"),
           CompleterType == "TA",
           CharterSchool == "All",
           AlternativeSchoolAccountabilityStatus == "All",
           !is.na(`High School Completers`))  %>%
    mutate(PercentUC = `Enrolled UC (12 Months)`/`High School Completers`,
           PercentCSU = `Enrolled CSU (12 Months)`/`High School Completers`,
           PercentCC = `Enrolled CCC (12 Months)`/`High School Completers`,
           PercentPrivate = `Enrolled In-State Private (2 and 4 Year) (12 Months)`/`High School Completers`,
           PercentOutCA = `Enrolled Out-of-State (12 Months)`/`High School Completers`,
           PercentTotal = PercentUC+PercentCSU+PercentCC+PercentPrivate+PercentOutCA,
           name = paste0(DistrictName," - ",SchoolName)) %>% 
    mutate(name = fct_reorder(name, PercentTotal))

ggplot(cgr.gender) +
    geom_col( aes(x = name,
                  y = `College Going Rate - Total (12 Months)`,
                  fill = ReportingCategory ),
              position = position_dodge2(preserve = "single")) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_hc() +
    labs(title = "Female students attend college at a higher rate than male students almost everywhere",
         x = "",
         fill = "Gender2") + 
    scale_fill_brewer(palette="Dark2",name = "Gender", labels = c("Female", "Male"))

ggsave("Gender.png", width = 14, height = 10, dpi = 500)


#### Graph by Ethnicity -------

cgr.eth <- cgr %>%
    filter(str_detect(CountyName, "Monterey"),
           ReportingCategory %in% c("RB", "RA", "RF", "RH", "RP", "RT", "RW"),
           AggregateLevel  %in% c("C","S"),  # School
           CompleterType == "TA",
           CharterSchool == "No",
           AlternativeSchoolAccountabilityStatus == "All",
           !is.na(`High School Completers`))  %>%
    mutate(PercentUC = `Enrolled UC (12 Months)`/`High School Completers`,
           PercentCSU = `Enrolled CSU (12 Months)`/`High School Completers`,
           PercentCC = `Enrolled CCC (12 Months)`/`High School Completers`,
           PercentPrivate = `Enrolled In-State Private (2 and 4 Year) (12 Months)`/`High School Completers`,
           PercentOutCA = `Enrolled Out-of-State (12 Months)`/`High School Completers`,
           PercentTotal = PercentUC+PercentCSU+PercentCC+PercentPrivate+PercentOutCA,
           name = paste0(DistrictName," - \n",SchoolName)) %>% 
    mutate(name = fct_reorder(name, PercentTotal))

ggplot(cgr.eth) +
    geom_col( aes(x = name,
                  y = `College Going Rate - Total (12 Months)`,
                  fill = ReportingCategory ),
              position = position_dodge2(preserve = "single")) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_hc()  +
    labs(title = "Asian students attend college at a higher rate while Latino and Pacific Islander students attend at a lower rate in Monterey County",
      x = "",
      fill = "Gender2")  +
     scale_fill_brewer(palette="Dark2",
                   name = "Ethnicity",
                       labels = c("Asian", "African American", "Filipino", "Hispanic or Latino", "Pacific Islander" ,"Two or More Races", "White"))

ggsave("Ethnicity.png", width = 14, height = 10, dpi = 500)




#### Graph by A-G -------

cgr.AG <- cgr %>%
    filter(str_detect(CountyName, "Monterey"),
           AggregateLevel  %in% c("C","S"),  # School
           ReportingCategory %in% c("TA"),
           CompleterType %in% c("AGY","AGN"),
           CharterSchool == "All",
           AlternativeSchoolAccountabilityStatus == "All",
           !is.na(`High School Completers`))  %>%
    mutate(PercentUC = `Enrolled UC (12 Months)`/`High School Completers`,
           PercentCSU = `Enrolled CSU (12 Months)`/`High School Completers`,
           PercentCC = `Enrolled CCC (12 Months)`/`High School Completers`,
           PercentPrivate = `Enrolled In-State Private (2 and 4 Year) (12 Months)`/`High School Completers`,
           PercentOutCA = `Enrolled Out-of-State (12 Months)`/`High School Completers`,
           PercentTotal = PercentUC+PercentCSU+PercentCC+PercentPrivate+PercentOutCA,
           name = paste0(DistrictName," - ",SchoolName)) %>% 
    mutate(name = fct_reorder(name, PercentTotal))

ggplot(cgr.AG) +
    geom_col( aes(x = name,
                  y = `College Going Rate - Total (12 Months)`,
                  fill = CompleterType ),
              position = position_dodge2(preserve = "single")) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_hc() +
    labs(title = "Female students attend college at a higher rate than male students almost everywhere",
         x = "",
         fill = "Gender2") + 
    scale_fill_brewer(palette="Dark2",name = "", labels = c("Met A-G", "Not Meet A-G"))

ggsave("A-G.png", width = 14, height = 10, dpi = 500)
