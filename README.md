The COVID-19 pandemic and microbial science
================
Megan Frederickson and Aspen Reese
21/06/2021

## How has the COVID-19 pandemic affected preprints and grants on microbes, microbiomes, and coronaviruses specifically?

We used these packages:

``` r
#Load packages
library(tidyverse) 
library(lubridate)
library(knitr)
library(cowplot)
#install.packages("devtools") 
#devtools::install_github("nicholasmfraser/rbiorxiv") 
library(rbiorxiv) 
library(medrxivr)
library(anytime)
library(car)
library(lme4)
library(scales)
#library(rcrossref)
```

# bioRxiv preprints

We scraped submission data from bioRxiv, the main preprint server for
biology, for Jan. 1, 2018-Jun. 15, 2021. We used the rbiorxiv package,
see:

Fraser, N (2020). rbiorxiv. R package,
<https://github.com/nicholasmfraser/rbiorxiv>

This code takes a while to run, so it does not run when rendering this
markdown document (but is reproduced here for transparency).

``` r
#Not run
#Get all submissions between Jan 1, 2021 and Jun 15, 2021
df.2021 <- biorxiv_content(from = "2021-01-01", to = "2021-06-15", limit = "*", format = "df")
write.csv(df.2021, "Data/biorxiv_2021_data.csv")

#Get all submissions between Jan 1, 2020 and December 31, 2020
df.2020.1 <- biorxiv_content(from = "2020-01-01", to = "2020-04-30", limit = "*", format = "df")
df.2020.2 <- biorxiv_content(from = "2020-05-01", to = "2020-08-31", limit = "*", format = "df")
df.2020.3 <- biorxiv_content(from = "2020-09-01", to = "2020-10-31", limit = "*", format = "df")
df.2020.4 <- biorxiv_content(from = "2020-11-01", to = "2020-11-30", limit = "*", format = "df")
df.2020.5 <- biorxiv_content(from = "2020-12-01", to = "2020-12-15", limit = "*", format = "df")
df.2020.6 <- biorxiv_content(from = "2020-12-16", to = "2020-12-31", limit = "*", format = "df")
df.2020 <- rbind(df.2020.1, df.2020.2, df.2020.3, df.2020.4, df.2020.5, df.2020.6)
write.csv(df.2020, "Data/biorxiv_2020_data.csv")

#Get all submissions between Jan 1, 2019 and December 31, 2019
df.2019.1 <- biorxiv_content(from = "2019-01-01", to = "2019-04-30", limit = "*", format = "df")
df.2019.2 <- biorxiv_content(from = "2019-05-01", to = "2019-08-31", limit = "*", format = "df")
df.2019.3 <- biorxiv_content(from = "2019-09-01", to = "2019-12-31", limit = "*", format = "df")
df.2019 <- rbind(df.2019.1, df.2019.2, df.2019.3)
write.csv(df.2019, "Data/biorxiv_2019_data.csv")

#Get all submissions between Jan 1, 2018 and December 31, 2018
df.2018.1 <- biorxiv_content(from = "2018-01-01", to = "2018-04-30", limit = "*", format = "df")
df.2018.2 <- biorxiv_content(from = "2018-05-01", to = "2018-08-31", limit = "*", format = "df")
df.2018.3 <- biorxiv_content(from = "2018-09-01", to = "2018-12-31", limit = "*", format = "df")
df.2018 <- rbind(df.2018.1, df.2018.2, df.2018.3)
write.csv(df.2018, "Data/biorxiv_2018_data.csv")
```

How many bioRxiv preprints are about SARS-CoV-2, COVID-19, or other
coronaviruses, versus microbiomes or other microbes?

``` r
#Compile dataset
df.2018 <- read.csv("Data/biorxiv_2018_data.csv")
df.2019 <- read.csv("Data/biorxiv_2019_data.csv")
df.2020 <- read.csv("Data/biorxiv_2020_data.csv")
df.2021 <- read.csv("Data/biorxiv_2021_data.csv")
df.full <- rbind(df.2018, df.2019, df.2020, df.2021[,-12])

#Search abstracts for terms
df.full$COVID.in.abstract <- grepl('SARS-CoV-2|COVID|coronavirus', df.full$abstract, ignore.case=TRUE)
df.full$microbiome.in.abstract <- grepl('microbiome|microbial community|microbial communities', df.full$abstract, ignore.case=TRUE)
df.full$microbe.in.abstract <- grepl('microbe|bacteria|bacterium|virus|archaea|SARS-CoV-2|COVID|coronavirus|microbiome|microbial community|fungus|fungi', df.full$abstract, ignore.case=TRUE)

#Search titles for terms
df.full$COVID.in.title <- grepl('SARS-CoV-2|COVID|coronavirus', df.full$title, ignore.case=TRUE)
df.full$microbiome.in.title <- grepl('microbiome|microbial community|microbial communities', df.full$title, ignore.case=TRUE)
df.full$microbe.in.title <- grepl('microbe|bacteria|bacterium|virus|archaea|SARS-CoV-2|COVID|coronavirus|microbiome|microbial community|fungus|fungi', df.full$title, ignore.case=TRUE)
```

There are 146155 bioRxiv preprints in the dataset, which spans Jan. 1,
2018 to Jun. 15, 2021. Of these, 5664 mention SARS-CoV-2, COVID-19, or
coronavirus in the abstract, or 3.88%. In contrast, 27878 mention any
microbe, including bacteria, fungi, archaea, and viruses, or 19.07%, and
3845 mention the microbiome or microbial communities, or 2.63%

``` r
df.full$week2 <- cut(as.POSIXct(df.full$date), "week") #Bin by week
sum.by.week <- df.full %>% group_by(week2) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.preprints = n() - n.covid.preprints - n.microbiome.preprints, n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by week
sum.by.week$week2 <- as.Date(sum.by.week$week2) #Make weeks dates
sum.by.week <- subset(sum.by.week, week2 < "2021-06-14") #Remove last (incomplete) week

sum.by.week.long <- gather(sum.by.week, type, number, n.covid.preprints:n.preprints) #Make wide data long

#Total preprints through time
p1 <- ggplot(data=subset(sum.by.week.long, type=="n.preprints"))+geom_line(aes(x=week2, y=number))+theme_cowplot()+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("bioRxiv preprints", subtitle="All preprints")
p1
```

![](README_files/figure-gfm/bioRxiv%20figure-1.png)<!-- -->

``` r
p2 <- ggplot(data=subset(sum.by.week.long, type != "n.other.preprints" & type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=week2)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_discrete(name = "Preprint type", labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("bioRxiv preprints")+theme(legend.position = "top")+guides(fill = guide_legend(nrow = 3))
p2
```

![](README_files/figure-gfm/bioRxiv%20figure-2.png)<!-- -->

``` r
save_plot("bioRxiv COVID vs. microbiome preprints_alternate.pdf", p2, base_width=8, base_height=4, dpi=300)
```

Subset by sub-discipline with bioRxiv

``` r
#Check which categories have a lot of covid preprints
sum.by.category <- df.full %>% group_by(category) %>% summarize(n=n(), n.covid = sum(COVID.in.abstract, na.rm=TRUE))
unique(subset(sum.by.category, n.covid >= 25)$category) #Categories with at least 25 COVID preprints
```

    ##  [1] "biochemistry"                          
    ##  [2] "bioengineering"                        
    ##  [3] "bioinformatics"                        
    ##  [4] "biophysics"                            
    ##  [5] "cell biology"                          
    ##  [6] "ecology"                               
    ##  [7] "evolutionary biology"                  
    ##  [8] "genetics"                              
    ##  [9] "genomics"                              
    ## [10] "immunology"                            
    ## [11] "microbiology"                          
    ## [12] "molecular biology"                     
    ## [13] "neuroscience"                          
    ## [14] "pathology"                             
    ## [15] "pharmacology and toxicology"           
    ## [16] "scientific communication and education"
    ## [17] "systems biology"

``` r
sum.by.week <- df.full[df.full$category %in% unique(subset(sum.by.category, n.covid >= 25)$category), ] %>% group_by(week2, category) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.preprints = n() - n.covid.preprints - n.microbiome.preprints, n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by week
```

    ## `summarise()` has grouped output by 'week2'. You can override using the `.groups` argument.

``` r
sum.by.week$week2 <- as.Date(sum.by.week$week2) #Make weeks dates
sum.by.week <- subset(sum.by.week, week2 < "2021-06-14") #Remove last (incomplete) week

sum.by.week.long <- gather(sum.by.week, type, number, n.covid.preprints:n.preprints) #Make wide data long
unique(sum.by.week.long$category)
```

    ##  [1] "biochemistry"                          
    ##  [2] "bioengineering"                        
    ##  [3] "bioinformatics"                        
    ##  [4] "biophysics"                            
    ##  [5] "cell biology"                          
    ##  [6] "ecology"                               
    ##  [7] "evolutionary biology"                  
    ##  [8] "genetics"                              
    ##  [9] "genomics"                              
    ## [10] "immunology"                            
    ## [11] "microbiology"                          
    ## [12] "molecular biology"                     
    ## [13] "neuroscience"                          
    ## [14] "pathology"                             
    ## [15] "systems biology"                       
    ## [16] "pharmacology and toxicology"           
    ## [17] "scientific communication and education"

``` r
sum.by.week.long$category <- gsub("scientific communication and education", "scientific communication\nand education", sum.by.week.long$category)
sum.by.week.long$category <- gsub("pharmacology and toxicology", "pharmacology and\ntoxicology", sum.by.week.long$category)

p3 <- ggplot(data=subset(sum.by.week.long, type != "n.other.preprints" & type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=week2)) + geom_bar(position="stack", stat="identity")+scale_fill_discrete(name = "Preprint type", labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("bioRxiv preprints")+facet_wrap(~category, scales='free')+theme_bw()+theme(legend.position = "top", panel.grid = element_blank(), strip.text = element_text(size=7))+guides(fill = guide_legend(nrow = 3))
p3
```

![](README_files/figure-gfm/bioRxiv%20sub-discipline%20figure-1.png)<!-- -->

``` r
save_plot("bioRxiv COVID vs. microbiome preprints by category_alternate.pdf", p3, base_width=8, base_height=10, dpi=300)
```

# medRxiv preprints

We also scraped submission data from medRxiv.

``` r
#Not run
df.med.2019 <- mx_api_content(from_date = "2019-01-01", to_date = "2019-12-31")
df.med.2020 <- mx_api_content(from_date = "2020-01-01", to_date = "2020-12-31") #Wow there has been HUGE growth in medRxiv
df.med.2021 <- mx_api_content(from_date = "2021-01-01", to_date = "2021-06-15")
write.csv(df.med.2019, "Data/medrxiv_2019_data.csv")
write.csv(df.med.2020, "Data/medrxiv_2020_data.csv")
write.csv(df.med.2021, "Data/medrxiv_2021_data.csv")
```

``` r
df.med.2019 <- read.csv("Data/medrxiv_2019_data.csv")
df.med.2020 <- read.csv("Data/medrxiv_2020_data.csv")
df.med.2021 <- read.csv("Data/medrxiv_2021_data.csv")
df.med.full <- rbind(df.med.2019, df.med.2020, df.med.2021[, -11])

df.med.full$COVID.in.abstract <- grepl('SARS-CoV-2|COVID|coronavirus', df.med.full$abstract, ignore.case=TRUE)
df.med.full$microbiome.in.abstract <- grepl('microbiome|microbial community|microbial communities', df.med.full$abstract, ignore.case=TRUE)
df.med.full$microbe.in.abstract <- grepl('microbe|bacteria|bacterium|virus|archaea|SARS-CoV-2|COVID|coronavirus|microbiome|microbial community|fungus|fungi', df.med.full$abstract, ignore.case=TRUE)

sum(df.med.full$COVID.in.abstract)
```

    ## [1] 17416

``` r
sum(df.med.full$microbiome.in.abstract)
```

    ## [1] 168

``` r
df.med.full$week2 <- cut(as.POSIXct(df.med.full$date), "week")

sum.by.week <- df.med.full %>% group_by(week2) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.preprints = n() - n.covid.preprints - n.microbiome.preprints, n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by week
sum.by.week$week2 <- as.Date(sum.by.week$week2) #Make weeks dates
sum.by.week <- subset(sum.by.week, week2 < "2021-06-14") #Remove last (incomplete) week

sum.by.week.long <- gather(sum.by.week, type, number, n.covid.preprints:n.preprints) #Make wide data long

#Total preprints through time
p4 <- ggplot(data=subset(sum.by.week.long, type=="n.preprints"))+geom_line(aes(x=week2, y=number))+theme_cowplot()+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("medRxiv preprints", subtitle="All preprints")
p4
```

![](README_files/figure-gfm/medRxiv%20figure-1.png)<!-- -->

``` r
p5 <- ggplot(data=subset(sum.by.week.long, type != "n.other.preprints" & type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=week2)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_discrete(name = "Preprint type", labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("medRxiv preprints")+theme(legend.position = "top")+guides(fill = guide_legend(nrow = 3))
p5
```

![](README_files/figure-gfm/medRxiv%20figure-2.png)<!-- -->

``` r
save_plot("medRxiv COVID vs. microbiome preprints_alternate.pdf", p5, base_width=8, base_height=4, dpi=300)
```
