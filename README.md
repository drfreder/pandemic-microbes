The COVID-19 pandemic and microbial science
================
Megan Frederickson and Aspen Reese
08/20/2021

This GitHub repo contains the code for all analyses in:

Frederickson ME, Reese A. 2021. Microbial ecology and evolution are
essential for understanding pandemics. mBio, accepted pending minor
revisions.

We quantified how many bioRxiv (<https://www.biorxiv.org/>) and medRxiv
(<https://www.medrxiv.org/>) preprints and NIH
(<https://reporter.nih.gov/>) project grants have involved COVID-19,
SARS-CoV-2, or coronaviruses between Jan 2018 and May/June 2021. For
comparison, we also quantified how many have involved the microbiome (or
“microbial community”) or any other microbe.

We used these packages:

``` r
#Load packages
library(tidyverse) 
library(lubridate)
library(knitr)
library(cowplot)
#install.packages("devtools") 
#devtools::install_github("nicholasmfraser/rbiorxiv") #To install rbiorxiv package
library(rbiorxiv) 
library(medrxivr)
library(anytime)
library(car)
library(lme4)
library(scales)
#devtools::install_github("katiejolly/nationalparkcolors") #To install nationalparkcolors package
library(nationalparkcolors)
```

## bioRxiv preprints

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
coronaviruses, versus microbiomes or other microbes? We performed
case-insensitive searches of abstracts for “SARS-CoV-2”, “COVID”,
“coronavirus”, and “2019-nCov” and compared the results to
case-insensitive searches of abstracts for “microbiome”, “microbial
community”, and “microbial communities” and singular and plural forms of
“bacteria”, “archaea”, “fungi”, and “virus”.

``` r
#Compile dataset
df.2018 <- read.csv("Data/biorxiv_2018_data.csv")
df.2019 <- read.csv("Data/biorxiv_2019_data.csv")
df.2020 <- read.csv("Data/biorxiv_2020_data.csv")
df.2021 <- read.csv("Data/biorxiv_2021_data.csv")
df.full <- rbind(df.2018, df.2019, df.2020, df.2021[,-12])
df.full <- df.full[!duplicated(df.full),] #Remove duplicates

#Set search terms
covid <- 'SARS-CoV-2|COVID|coronavirus|2019-nCov'
microbiome <- 'microbiome|microbial community|microbial communities'
microbes <- 'microbe|bacteria|bacterium|virus|archaea|SARS-CoV-2|COVID|coronavirus|microbiome|microbial community|fungus|fungi' #All microbial search terms together 

#Search abstracts for terms
df.full$COVID.in.abstract <- grepl(covid, df.full$abstract, ignore.case=TRUE)
df.full$microbiome.in.abstract <- grepl(microbiome, df.full$abstract, ignore.case=TRUE)
df.full$microbe.in.abstract <- grepl(microbes, df.full$abstract, ignore.case=TRUE) 
```

There are 146155 bioRxiv preprints in the dataset, which spans Jan. 1,
2018 to Jun. 15, 2021. Of these, 5669 mention SARS-CoV-2, COVID-19, or
coronavirus in the abstract, or 3.88%. In contrast, 27878 mention any
microbe, including bacteria, fungi, archaea, and viruses, or 19.07%, and
3845 mention the microbiome or microbial communities, or 2.63%

How many total preprints have been submitted to bioRxiv through time?

``` r
df.full$week2 <- cut(as.POSIXct(df.full$date), "week") #Bin by week
df.full$month <- cut(as.POSIXct(df.full$date), "month") #Bin by month

#By week
sum.by.week <- df.full %>% group_by(week2) %>% summarize(n.preprints = n()) #Summarize by week
sum.by.week$week2 <- as.Date(sum.by.week$week2) #Make weeks dates
sum.by.week <- subset(sum.by.week, week2 < "2021-06-14") #Remove last (incomplete) week

#Total preprints through time
p1 <- ggplot(data=sum.by.week)+geom_line(aes(x=week2, y=n.preprints))+theme_cowplot()+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("bioRxiv preprints", subtitle="All preprints")+scale_x_date(limits=c(as.Date("2018-01-01"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p1
```

![](README_files/figure-gfm/bioRxiv%20total%20preprints%20figure-1.png)<!-- -->

How many bioRxiv preprints are there on COVID-19, SARS-CoV-2, or
coronaviruses, compared to other microbial science topics?

``` r
#By month
sum.by.month <- df.full %>% group_by(month) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by month
#Note that n.microbe.preprints includes COVID and microbiome preprints, while n.other.microbe.preprints subtracts these out
sum.by.month$month <- as.Date(sum.by.month$month) #Make months dates
sum.by.month <- subset(sum.by.month, month < "2021-06-01") #Remove last (incomplete) month
sum.by.month.long <- gather(sum.by.month, type, number, n.covid.preprints:n.preprints) #Make wide data long

#Make figure
p2 <- ggplot(data=subset(sum.by.month.long, type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=month)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_manual(values=rev(park_palette("Badlands", 3)), labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no.)")+xlab("Month")+ggtitle("bioRxiv preprints")+theme(legend.position = "none", legend.title=element_blank())+guides(fill = guide_legend(nrow = 3))+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p2
```

![](README_files/figure-gfm/bioRxiv%20main%20figure-1.png)<!-- -->

How do these patterns break down by author-identified sub-discipline in
the bioRxiv dataset?

``` r
#Check which categories have a lot of covid preprints
sum.by.category <- df.full %>% group_by(category) %>% summarize(n.covid = sum(COVID.in.abstract, na.rm=TRUE))

#By month and category, for categories with at least 25 COVID-19 preprints
sum.by.month <- df.full[df.full$category %in% unique(subset(sum.by.category, n.covid >= 25)$category), ] %>% group_by(month, category) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by month
sum.by.month$month <- as.Date(sum.by.month$month) #Make months dates
sum.by.month <- subset(sum.by.month, month < "2021-06-01") #Remove last (incomplete) month

sum.by.month.long <- gather(sum.by.month, type, number, n.covid.preprints:n.preprints) #Make wide data long
missing.fields <- sum.by.category[which(!sum.by.category$category %in% sum.by.month.long$category), "category"] #Fields not in figure
sum.by.month.long$category <- gsub("scientific communication and education", "scientific communication\nand education", sum.by.month.long$category) #Put long categories on two lines
sum.by.month.long$category <- gsub("pharmacology and toxicology", "pharmacology and\ntoxicology", sum.by.month.long$category) #Put long categories on two lines

p3 <- ggplot(data=subset(sum.by.month.long, type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=month)) + geom_bar(position="stack", stat="identity")+scale_fill_manual(values=rev(park_palette("Badlands", 3)), labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no.)")+xlab("Month")+ggtitle("bioRxiv preprints")+facet_wrap(~category, scales = 'free')+theme_cowplot()+theme(axis.text.x = element_text(angle = 15), legend.position = c(0.419, 0.16), panel.grid = element_blank(), legend.justification='left', legend.title=element_blank(), strip.text = element_text(size=9), axis.text=element_text(size=9))+guides(fill = guide_legend(nrow = 3))+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p3
```

![](README_files/figure-gfm/bioRxiv%20sub-discipline%20figure-1.png)<!-- -->

``` r
save_plot("figureS1.pdf", p3, base_width=10, base_height=8, dpi=300)
```

This figure excludes sub-disciplines with fewer than 25 preprints on
SARS-CoV-2, COVID-19, or coronaviruses, specifically:

| category                      |
|:------------------------------|
| animal behavior and cognition |
| cancer biology                |
| clinical trials               |
| developmental biology         |
| epidemiology                  |
| paleontology                  |
| physiology                    |
| plant biology                 |
| synthetic biology             |
| zoology                       |

Note that the reason that epidemiology and clinical trials have few
COVID-19 papers in the bioRxiv database is that these are overwhelmingly
submitted to medRxiv (see below).

We also want to visualize the data on sub-fields in a more concise form
for the main text.

``` r
df.full$month <- as.Date(df.full$month)
sum.by.category.covid <- subset(df.full, COVID.in.abstract & month < "2021-06-01") %>% group_by(category) %>% summarize(n=n())
sum.by.category.covid$type <- "COVID-19, SARS-CoV-2, or coronavirus"
sum.by.category.covid$percent <- sum.by.category.covid$n/sum(sum.by.category.covid$n)*100
sum.by.category.microbiome <- subset(df.full, microbiome.in.abstract & month < "2021-06-01") %>% group_by(category) %>% summarize(n=n())
sum.by.category.microbiome$type <- "Microbiome or microbial community"
sum.by.category.microbiome$percent <- sum.by.category.microbiome$n/sum(sum.by.category.microbiome$n)*100
sum.by.category.other.microbe <- subset(df.full, !COVID.in.abstract & !microbiome.in.abstract & microbe.in.abstract & month < "2021-06-01") %>% group_by(category) %>% summarize(n=n())
sum.by.category.other.microbe$type <- "Other microbe"
sum.by.category.other.microbe$percent <- sum.by.category.other.microbe$n/sum(sum.by.category.other.microbe$n)*100
sum.by.category <- rbind(sum.by.category.covid, sum.by.category.microbiome, sum.by.category.other.microbe)
sum.by.category$category2 <- ifelse(sum.by.category$percent > 6 | sum.by.category$category == "ecology" | sum.by.category$category == "evolutionary biology", sum.by.category$category, "other")
sum.by.category$category2 <- ordered(sum.by.category$category2, levels=c("ecology", "evolutionary biology", "biochemistry", "bioinformatics", "biophysics", "genomics", "immunology", "microbiology", "molecular biology", "other"))

p4 <- ggplot(data=sum.by.category, aes(x=type, y=n, fill=category2))+geom_bar(position="fill", stat="identity")+theme_cowplot()+ylab("Preprints (prop.)")+xlab("Topic")+scale_fill_manual(values=c(park_palette("SmokyMountains")[[2]], park_palette("SmokyMountains")[[4]], "#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"))+theme(legend.title=element_blank())+scale_x_discrete(labels = wrap_format(20))
p4
```

![](README_files/figure-gfm/bioRxiv%20sub-discipline%20stacked%20bar%20graph%20figure-1.png)<!-- -->

``` r
save_plot("figure2.tiff", p4, base_width=8, base_height=6, bg="white", dpi=300)
```

Ecology and evolutionary biology account for just 3.6% of preprints on
COVID-19, SARS-CoV-2, or coronaviruses, compared to 15.5% of preprints
on microbiomes and microbial communities and 10.8% of preprints on other
microbes.

## medRxiv preprints

We also scraped submission data from medRxiv, for the same dates
(Jan. 1, 2018-Jun. 15, 2021). Again, this code is not run when rendering
this markdown document, but it is reproduced here for transparency.

``` r
#Not run
df.med.2019 <- mx_api_content(from_date = "2019-01-01", to_date = "2019-12-31")
df.med.2020 <- mx_api_content(from_date = "2020-01-01", to_date = "2020-12-31") #Wow there has been HUGE growth in medRxiv
df.med.2021 <- mx_api_content(from_date = "2021-01-01", to_date = "2021-06-15")
write.csv(df.med.2019, "Data/medrxiv_2019_data.csv")
write.csv(df.med.2020, "Data/medrxiv_2020_data.csv")
write.csv(df.med.2021, "Data/medrxiv_2021_data.csv")
```

How many medRxiv preprints are about SARS-CoV-2, COVID-19, or other
coronaviruses, versus microbiomes or other microbes? We used the same
case-insensitive search terms here as above.

``` r
df.med.2019 <- read.csv("Data/medrxiv_2019_data.csv")
df.med.2020 <- read.csv("Data/medrxiv_2020_data.csv")
df.med.2021 <- read.csv("Data/medrxiv_2021_data.csv")
df.med.full <- rbind(df.med.2019, df.med.2020, df.med.2021[, -11])
df.med.full <- df.med.full[!duplicated(df.med.full),] #Remove duplicates

df.med.full$COVID.in.abstract <- grepl(covid, df.med.full$abstract, ignore.case=TRUE)
df.med.full$microbiome.in.abstract <- grepl(microbiome, df.med.full$abstract, ignore.case=TRUE)
df.med.full$microbe.in.abstract <- grepl(microbes, df.med.full$abstract, ignore.case=TRUE)
```

There are 27601 medRxiv preprints in the dataset, which spans Jan. 1,
2018 to Jun. 15, 2021. Of these, 17437 mention SARS-CoV-2, COVID-19, or
coronavirus in the abstract, or 63.18%. In contrast, 18262 mention any
microbe, including bacteria, fungi, archaea, and viruses, or 66.16%, and
168 mention the microbiome or microbial communities, or 0.61%

How many total preprints have been submitted to medRxiv through time?
Note that medRxiv was launched only in June 2019.

``` r
df.med.full$week2 <- cut(as.POSIXct(df.med.full$date), "week")
df.med.full$month <- cut(as.POSIXct(df.med.full$date), "month")

#By week
sum.by.week <- df.med.full %>% group_by(week2) %>% summarize(n.preprints = n()) #Summarize by week
sum.by.week$week2 <- as.Date(sum.by.week$week2) #Make weeks dates
sum.by.week <- subset(sum.by.week, week2 < "2021-06-14") #Remove last (incomplete) week

#Total preprints through time
p5 <- ggplot(data=sum.by.week)+geom_line(aes(x=week2, y=n.preprints))+theme_cowplot()+ylab("Preprints (no./week)")+xlab("Date")+ggtitle("medRxiv preprints", subtitle="All preprints")+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p5
```

![](README_files/figure-gfm/medRxiv%20total%20preprints%20figure-1.png)<!-- -->

How many medRxiv preprints are there on COVID-19, SARS-CoV-2, or
coronaviruses, compared to other microbial science topics?

``` r
#By month
sum.by.month <- df.med.full %>% group_by(month) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by month
sum.by.month$month <- as.Date(sum.by.month$month) #Make months dates
sum.by.month <- subset(sum.by.month, month < "2021-06-01") #Remove last (incomplete) month
sum.by.month.long <- gather(sum.by.month, type, number, n.covid.preprints:n.preprints) #Make wide data long

p6 <- ggplot(data=subset(sum.by.month.long, type != "n.other.preprints" & type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=month)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_manual(values=rev(park_palette("Badlands", 3)),  labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Preprints (no.)")+xlab("Month")+ggtitle("medRxiv preprints")+theme(legend.position = c(0.022, 0.85), legend.title=element_blank(), legend.text=element_text(size=10))+guides(fill = guide_legend(nrow = 3))+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p6
```

![](README_files/figure-gfm/medRxiv%20main%20figure-1.png)<!-- -->

# NIH Research Project Grants

We also compiled data on US National Institutes of Health (NIH) research
project grants over the same period, and searched their abstracts for
the same terms as above.

``` r
nih.2018 <- rbind(read_csv("Data/NIH_2018_part1.csv"), read_csv("Data/NIH_2018_part2.csv"), read_csv("Data/NIH_2018_part1.csv"), read_csv("Data/NIH_2018_part2.csv"), read_csv("Data/NIH_2018_part3.csv"), read_csv("Data/NIH_2018_part4.csv"), read_csv("Data/NIH_2018_part5.csv"), read_csv("Data/NIH_2018_part6.csv"), read_csv("Data/NIH_2018_part7.csv"), read_csv("Data/NIH_2018_part8.csv"), read_csv("Data/NIH_2018_part9.csv"), read_csv("Data/NIH_2018_part10.csv"), read_csv("Data/NIH_2018_part11.csv"))
nih.2019 <- rbind(read_csv("Data/NIH_2019_part1.csv"), read_csv("Data/NIH_2019_part2.csv"), read_csv("Data/NIH_2019_part3.csv"), read_csv("Data/NIH_2019_part4.csv"), read_csv("Data/NIH_2019_part5.csv"))
nih.2020 <- rbind(read_csv("Data/NIH_2020_part1.csv"), read_csv("Data/NIH_2020_part2.csv"), read_csv("Data/NIH_2020_part3.csv"), read_csv("Data/NIH_2020_part4.csv"), read_csv("Data/NIH_2020_part5.csv"))
nih.2021 <- rbind(read_csv("Data/NIH_2021_part1.csv"), read_csv("Data/NIH_2021_part2.csv"))
df.nih.full <- rbind(nih.2018, nih.2019, nih.2020, nih.2021)
  
#Search for COVID-19, microbiome, and other microbe search terms
df.nih.full$COVID.in.abstract <- grepl(covid, df.nih.full$`Project Abstract`, ignore.case=TRUE)
df.nih.full$microbiome.in.abstract <- grepl(microbiome, df.nih.full$`Project Abstract`, ignore.case=TRUE)
df.nih.full$microbe.in.abstract <- grepl(microbes, df.nih.full$`Project Abstract`, ignore.case=TRUE)
```

There are 199854 NIH Research Project grants in the dataset, which spans
Jan. 1, 2018 to Jun. 15, 2021. Of these, 1407 mention SARS-CoV-2,
COVID-19, or coronavirus in the abstract, or 0.7%. In contrast, 30282
mention any microbe, including bacteria, fungi, archaea, and viruses, or
15.15%, and 3849 mention the microbiome or microbial communities, or
1.93%.

The NIH dataset has a field for NIH COVID-19 Response, which according
to the website “returns projects awarded to study COVID-19 and related
topics, as funder under: RegCV NIH regular appropriations funding, CV
Coronavirus Preparedness and Response Supplemental Appropriations Act,
2020, C3 CARES Act (Coronavirus Aid, Relief, and Economic Security Act),
C4 Paycheck Protection Program and Health Care Enhancement Act, C5
Coronavirus Response and Relief Supplemental Appropriations Act, 2021.”
We checked the correspondence between the COVID-19 search terms we used
and the NIH COVID-19 Response field.

``` r
#Adjust date fields
df.nih.full$date <-gsub(" 12:00:00 AM", "", df.nih.full$`Award Notice Date`)
df.nih.full$date <-as.Date(df.nih.full$date, format="%m/%d/%Y")
df.nih.full$week2 <- cut(as.POSIXct(df.nih.full$date), "week")
df.nih.full$month <- cut(as.POSIXct(df.nih.full$date), "month")
df.nih.full$year <- cut(as.POSIXct(df.nih.full$date), "year")

#Check correspondence between COVID-19 search terms and NIH COVID-19 Response database category
df.nih.full$NIH.covid <- ifelse(!is.na(df.nih.full$`NIH COVID-19 Response`), TRUE, FALSE)
kable(df.nih.full %>% group_by(`NIH.covid`, COVID.in.abstract) %>% summarize (n=n()))
```

| NIH.covid | COVID.in.abstract |      n |
|:----------|:------------------|-------:|
| FALSE     | FALSE             | 198047 |
| FALSE     | TRUE              |    492 |
| TRUE      | FALSE             |    400 |
| TRUE      | TRUE              |    915 |

The earliest date of a grant through the NIH COVID-19 response is
2020-02-05, so some grants funded for research on coronaviruses (e.g.,
on SARS or MERS) are captured in our analysis, but are not part of the
NIH COVID-19 response. Similarly, not all grant abstracts that mention
COVID-19 are considered part of the NIH COVID-19 response.

How many total research project grants have been awarded by NIH over
this time period?

``` r
#By month
sum.by.month <- df.nih.full %>% group_by(month) %>% summarize(n.covid.preprints = sum(COVID.in.abstract, na.rm=TRUE), n.microbiome.preprints = sum(microbiome.in.abstract, na.rm=TRUE), n.microbe.preprints = sum(microbe.in.abstract, na.rm=TRUE), n.other.preprints = n() - n.covid.preprints - n.microbiome.preprints, n.other.microbe.preprints = n.microbe.preprints-n.covid.preprints-n.microbiome.preprints, n.preprints = n()) #Summarize by month
sum.by.month$month <- as.Date(sum.by.month$month) #Make months dates
sum.by.month <- subset(sum.by.month, month < "2021-06-01") #Remove last (incomplete) month
sum.by.month.long <- gather(sum.by.month, type, number, n.covid.preprints:n.preprints) #Make wide data long

#Total grants through time
p7 <- ggplot(data=subset(sum.by.month.long, type=="n.preprints"))+geom_line(aes(x=month, y=number))+theme_cowplot()+ylab("Grants (no./month)")+xlab("Date")+ggtitle("NIH", subtitle="Research Project Grants")+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p7
```

![](README_files/figure-gfm/NIH%20total%20grants%20figure-1.png)<!-- -->

How many NIH research project grants are there on COVID-19, SARS-CoV-2,
or coronaviruses, compared to other microbial science topics?

``` r
p8 <- ggplot(data=subset(sum.by.month.long, type != "n.other.preprints" & type != "n.microbe.preprints" & type != "n.preprints"), aes(fill=type, y=number, x=month)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_manual(values=rev(park_palette("Badlands", 3)),  labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Grants (no.)")+xlab("Month")+ggtitle("NIH grants")+theme(legend.position = "none", legend.title=element_blank())+guides(fill = guide_legend(nrow = 3))+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p8
```

![](README_files/figure-gfm/NIH%20grant%20number%20figure-1.png)<!-- -->

How many NIH grant dollars went to COVID-19, SARS-CoV-2, or
coronaviruses, compared to other microbial science topics?

``` r
sum.by.month.dollars <- df.nih.full %>% group_by(month, COVID.in.abstract) %>% summarize(dollars = sum(`Total Cost`, na.rm=TRUE)) #Summarize by month
sum.by.month.dollars$month <- as.Date(sum.by.month.dollars$month) #Make months dates
sum.by.month.dollars <- subset(sum.by.month.dollars, month < "2021-06-01" & COVID.in.abstract == TRUE) #Remove last (incomplete) month
sum.by.month.dollars.microbiome <- df.nih.full %>% group_by(month, microbiome.in.abstract) %>% summarize(dollars = sum(`Total Cost`, na.rm=TRUE)) #Summarize by month
sum.by.month.dollars.microbiome$month <- as.Date(sum.by.month.dollars.microbiome$month)
sum.by.month.dollars.microbiome <- subset(sum.by.month.dollars.microbiome, month < "2021-06-01" & microbiome.in.abstract == TRUE) #Remove last (incomplete) month
sum.by.month.dollars.other <- subset(df.nih.full, COVID.in.abstract == FALSE & microbiome.in.abstract == FALSE) %>% group_by(month, microbe.in.abstract) %>% summarize(dollars = sum(`Total Cost`, na.rm=TRUE)) #Summarize by month
sum.by.month.dollars.other$month <- as.Date(sum.by.month.dollars.other$month)
sum.by.month.dollars.other <- subset(sum.by.month.dollars.other, month < "2021-06-01" & microbe.in.abstract == TRUE) #Remove last (incomplete) month

sum.by.month.dollars$type <- "COVID-19"
sum.by.month.dollars.microbiome$type <- "Microbiome"
sum.by.month.dollars.other$type <- "Other"

dollars.by.month <- rbind(sum.by.month.dollars[, -2], sum.by.month.dollars.microbiome[, -2], sum.by.month.dollars.other[,-2])

p9 <- ggplot(data=dollars.by.month, aes(fill=type, y=dollars, x=month)) + geom_bar(position="stack", stat="identity")+theme_cowplot()+scale_fill_manual(values=rev(park_palette("Badlands", 3)),  labels = c("COVID-19, SARS-CoV-2, or coronavirus", "Microbiome or microbial community", "Other microbe"))+ylab("Amount (US$)")+xlab("Month")+ggtitle("NIH grants")+theme(legend.position = "none", legend.title=element_blank())+guides(fill = guide_legend(nrow = 3))+scale_x_date(limits=c(as.Date("2017-12-15"),as.Date("2021-06-15")), date_labels="%m-%Y")+geom_vline(xintercept=as.Date("2020-03-11"), linetype="dotted")
p9
```

![](README_files/figure-gfm/NIH%20grant%20dollars%20figure-1.png)<!-- -->

Put the bioRxiv, medRxiv, and NIH results together in a single figure.

``` r
p10 <- plot_grid(p2, p6, p9, nrow=3, align='v')
p10
```

![](README_files/figure-gfm/Omnibus%20figure-1.png)<!-- -->

``` r
save_plot("figure1.tiff", p10, base_width=6, base_height=8, bg="white", dpi=300)
```
