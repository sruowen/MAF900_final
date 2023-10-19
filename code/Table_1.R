rm(list=ls())
set.seed(123)

##Importing libraries
library(tidyverse)
library(RSQLite)
library(RPostgres)


                      ####Part1: Data Extraction####

#creating SQL database connection in the current project##
MAF900_data <- dbConnect(
  SQLite(),
  "data/MAF900_data.sqlite",
  extended_types = TRUE
)

##establishing wrds connection for the data
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user=)

crsp_monthly<- tbl(wrds, sql("select * from crsp_a_stock.msf")) |>
  filter(date >= '1926-01-01'  & date <= '2022-12-31') |>
  collect()     
crsp_monthly

##securities that only listed in NYSE i.e hexcd=1

crsp_monthly<- crsp_monthly |> 
  filter(hexcd==1) |> 
  collect() |> 
  unique() ##3total 2005941 obs

##saving data into local directory
dbWriteTable(MAF900_data,
             "crsp_monthly",
             value = crsp_monthly,
             overwrite = TRUE ) 
dbListTables(MAF900_data)

##importing data from the local repository
rm(list=ls())
set.seed(123)
library(RSQLite)
library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user=)

##filtering out the missing return values
crsp_monthly_db <- tbl(MAF900_data, "crsp_monthly")
crsp_monthly <- crsp_monthly_db |> filter(ret!="NA") |> 
  select(permno,date,hexcd,ret,shrout,prc) |> 
  collect()

##ff_factors monthly
##ff_3factors_monthly
temp <- tempfile(fileext = ".zip")
download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",temp)
temp1 <- unzip(temp, exdir = ".")

ff_3factors_monthly <- read.csv(temp1, skip=5, header = F) 
names(ff_3factors_monthly) <- c('dt', 'rmrf', 'smb', 'hml', 'rf')
unlink(temp)
unlink(temp1)
head(ff_3factors_monthly)
### Create date variable, filter the data for the sample period, convert chr data to numeric ###

ff_3factors_mon <- ff_3factors_monthly |> 
  filter(nchar(dt) == 6) |> 
  mutate(yr = str_sub(dt,1,4), mon= str_sub(dt,-2,-1),  
         date = make_date(year= yr, month = mon, day = 01), 
         mkt_excess = as.numeric(rmrf), smb = as.numeric(smb),
         hml = as.numeric(hml), rf = as.numeric(rf)) |> 
  filter(date >='1980-01-01' & date <= '2020-12-01') |> 
  select(c('date','mkt_excess','smb','hml','rf'))

dbWriteTable(MAF900_data,
             "ff_3factors_mon",
             value =ff_3factors_mon,
             overwrite = TRUE ) 
dbListTables(MAF900_data)

##calling the data
ff_3factors_mon_db <- tbl(MAF900_data, "ff_3factors_mon")
ff_3factors_mon <- ff_3factors_mon_db |> 
  collect()


###creating table1 formation, estimation and testing periods for portfolios###

##Creating period_1 (1926-29) portfolio formation
period_1_formation<- crsp_monthly |> 
  filter(date >= '1926-01-01'  & date <= '1929-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1927-33) portfolio formation
period_2_formation<- crsp_monthly |> 
  filter(date >= '1927-01-01'  & date <= '1933-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1931-37) portfolio formation
period_3_formation<- crsp_monthly |> 
  filter(date >= '1931-01-01'  & date <= '1937-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1935-41) portfolio formation
period_4_formation<- crsp_monthly |> 
  filter(date >= '1935-01-01'  & date <= '1941-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1939-45) portfolio formation
period_5_formation<- crsp_monthly |> 
  filter(date >= '1939-01-01'  & date <= '1945-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1943-49) portfolio formation
period_6_formation<- crsp_monthly |> 
  filter(date >= '1943-01-01'  & date <= '1949-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1947-53) portfolio formation
period_7_formation<- crsp_monthly |> 
  filter(date >= '1947-01-01'  & date <= '1953-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1951-57) portfolio formation
period_8_formation<- crsp_monthly |> 
  filter(date >= '1951-01-01'  & date <= '1957-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1955-61) portfolio formation
period_9_formation<- crsp_monthly |> 
  filter(date >= '1955-01-01'  & date <= '1961-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1959-65) portfolio formation
period_10_formation<- crsp_monthly |> 
  filter(date >= '1959-01-01'  & date <= '1965-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1963-69) portfolio formation
period_11_formation<- crsp_monthly |> 
  filter(date >= '1963-01-01'  & date <= '1969-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1967-73) portfolio formation
period_12_formation<- crsp_monthly |> 
  filter(date >= '1967-01-01'  & date <= '1973-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1971-77) portfolio formation
period_13_formation<- crsp_monthly |> 
  filter(date >= '1971-01-01'  & date <= '1977-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1975-81) portfolio formation
period_14_formation<- crsp_monthly |> 
  filter(date >= '1975-01-01'  & date <= '1981-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1979-85) portfolio formation
period_15_formation<- crsp_monthly |> 
  filter(date >= '1975-01-01'  & date <= '1981-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1983-89) portfolio formation
period_16_formation<- crsp_monthly |> 
  filter(date >= '1983-01-01'  & date <= '1989-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1987-93) portfolio formation
period_17_formation<- crsp_monthly |> 
  filter(date >= '1987-01-01'  & date <= '1993-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1991-97) portfolio formation
period_18_formation<- crsp_monthly |> 
  filter(date >= '1991-01-01'  & date <= '1997-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1995-2001) portfolio formation
period_19_formation<- crsp_monthly |> 
  filter(date >= '1995-01-01'  & date <= '2001-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (1999-2005) portfolio formation
period_20_formation<- crsp_monthly |> 
  filter(date >= '1999-01-01'  & date <= '2005-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (2003-2009) portfolio formation
period_21_formation<- crsp_monthly |> 
  filter(date >= '2003-01-01'  & date <= '2009-12-31') |>
  collect()## total 26899 obs

##Creating period_1 (2007-2013) portfolio formation
period_22_formation<- crsp_monthly |> 
  filter(date >= '2007-01-01'  & date <= '2013-12-31') |>
  collect()## total 26899 obs






##Creating period_1 (1930-34) portfolio estimation formation
period_1_estimation<- crsp_monthly |> 
  filter(date >= '1930-01-01'  & date <= '1934-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1934-38) portfolio estimation formation
period_2_estimation<- crsp_monthly |> 
  filter(date >= '1934-01-01'  & date <= '1938-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1938-42) portfolio estimation formation
period_3_estimation<- crsp_monthly |> 
  filter(date >= '1938-01-01'  & date <= '1942-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1942-46) portfolio estimation formation
period_4_estimation<- crsp_monthly |> 
  filter(date >= '1942-01-01'  & date <= '1946-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1946-50) portfolio estimation formation
period_5_estimation<- crsp_monthly |> 
  filter(date >= '1946-01-01'  & date <= '1950-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1950-54) portfolio estimation formation
period_6_estimation<- crsp_monthly |> 
  filter(date >= '1950-01-01'  & date <= '1954-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1954-58) portfolio estimation formation
period_7_estimation<- crsp_monthly |> 
  filter(date >= '1954-01-01'  & date <= '1958-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1958-62) portfolio estimation formation
period_8_estimation<- crsp_monthly |> 
  filter(date >= '1958-01-01'  & date <= '1962-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1962-66) portfolio estimation formation
period_9_estimation<- crsp_monthly |> 
  filter(date >= '1962-01-01'  & date <= '1966-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1966-70) portfolio estimation formation
period_10_estimation<- crsp_monthly |> 
  filter(date >= '1966-01-01'  & date <= '1970-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1970-74) portfolio estimation formation
period_11_estimation<- crsp_monthly |> 
  filter(date >= '1970-01-01'  & date <= '1974-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1974-78) portfolio estimation formation
period_12_estimation<- crsp_monthly |> 
  filter(date >= '1974-01-01'  & date <= '1978-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1978-82) portfolio estimation formation
period_13_estimation<- crsp_monthly |> 
  filter(date >= '1978-01-01'  & date <= '1982-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1982-86) portfolio estimation formation
period_14_estimation<- crsp_monthly |> 
  filter(date >= '1982-01-01'  & date <= '1986-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1986-90) portfolio estimation formation
period_15_estimation<- crsp_monthly |> 
  filter(date >= '1986-01-01'  & date <= '1990-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1990-94) portfolio estimation formation
period_16_estimation<- crsp_monthly |> 
  filter(date >= '1990-01-01'  & date <= '1994-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1994-98) portfolio estimation formation
period_17_estimation<- crsp_monthly |> 
  filter(date >= '1994-01-01'  & date <= '1998-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (1998-2002) portfolio estimation formation
period_18_estimation<- crsp_monthly |> 
  filter(date >= '1998-01-01'  & date <= '2002-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (2002-2006) portfolio estimation formation
period_19_estimation<- crsp_monthly |> 
  filter(date >= '2002-01-01'  & date <= '2006-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (2006-2010) portfolio estimation formation
period_20_estimation<- crsp_monthly |> 
  filter(date >= '2006-01-01'  & date <= '2010-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (2010-2014) portfolio estimation formation
period_21_estimation<- crsp_monthly |> 
  filter(date >= '2010-01-01'  & date <= '2014-12-31') |>
  collect()## total 40815 obs

##Creating period_1 (2014-2018) portfolio estimation formation
period_22_estimation<- crsp_monthly |> 
  filter(date >= '2014-01-01'  & date <= '2018-12-31') |>
  collect()## total 40815 obs





##Creating period_1 (1935-38) portfolio formation
period_1_testing<- crsp_monthly |> 
  filter(date >= '1935-01-01'  & date <= '1938-12-31') |>
  collect()## total 33687 obs


##Creating period_1 (1939-42) portfolio formation
period_2_testing<- crsp_monthly |> 
  filter(date >= '1939-01-01'  & date <= '1942-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1943-46) portfolio formation
period_3_testing<- crsp_monthly |> 
  filter(date >= '1943-01-01'  & date <= '1946-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1947-50) portfolio formation
period_4_testing<- crsp_monthly |> 
  filter(date >= '1947-01-01'  & date <= '1950-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1951-54) portfolio formation
period_5_testing<- crsp_monthly |> 
  filter(date >= '1951-01-01'  & date <= '1954-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1955-58) portfolio formation
period_6_testing<- crsp_monthly |> 
  filter(date >= '1955-01-01'  & date <= '1958-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1959-62) portfolio formation
period_7_testing<- crsp_monthly |> 
  filter(date >= '1959-01-01'  & date <= '1962-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1963-66) portfolio formation
period_8_testing<- crsp_monthly |> 
  filter(date >= '1963-01-01'  & date <= '1966-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1967-70) portfolio formation
period_9_testing<- crsp_monthly |> 
  filter(date >= '1967-01-01'  & date <= '1970-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1971-74) portfolio formation
period_10_testing<- crsp_monthly |> 
  filter(date >= '1971-01-01'  & date <= '1974-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1975-78) portfolio formation
period_11_testing<- crsp_monthly |> 
  filter(date >= '1975-01-01'  & date <= '1978-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1979-82) portfolio formation
period_12_testing<- crsp_monthly |> 
  filter(date >= '1979-01-01'  & date <= '1982-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1983-86) portfolio formation
period_13_testing<- crsp_monthly |> 
  filter(date >= '1983-01-01'  & date <= '1986-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1987-90) portfolio formation
period_14_testing<- crsp_monthly |> 
  filter(date >= '1987-01-01'  & date <= '1990-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1991-94) portfolio formation
period_15_testing<- crsp_monthly |> 
  filter(date >= '1991-01-01'  & date <= '1994-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1995-98) portfolio formation
period_16_testing<- crsp_monthly |> 
  filter(date >= '1995-01-01'  & date <= '1998-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (1999-2002) portfolio formation
period_17_testing<- crsp_monthly |> 
  filter(date >= '1999-01-01'  & date <= '2002-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (2003-2006) portfolio formation
period_18_testing<- crsp_monthly |> 
  filter(date >= '2003-01-01'  & date <= '2006-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (2007-2010) portfolio formation
period_19_testing<- crsp_monthly |> 
  filter(date >= '2007-01-01'  & date <= '2010-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (2011-2014) portfolio formation
period_20_testing<- crsp_monthly |> 
  filter(date >= '2011-01-01'  & date <= '2014-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (2015-2018) portfolio formation
period_21_testing<- crsp_monthly |> 
  filter(date >= '2015-01-01'  & date <= '2018-12-31') |>
  collect()## total 33687 obs

##Creating period_1 (2019-2022) portfolio formation
period_22_testing<- crsp_monthly |> 
  filter(date >= '2019-01-01'  & date <= '2022-12-31') |>
  collect()## total 33687 obs



## no. stock available for first month of the testing period i.e 1935-01
##separating month and year and creating another column
period_1_testing <- period_1_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count1_testing<-period_1_testing |> group_by(permno) |> 
  filter(year == 1935, month == 1) |> 
  unique()

period_2_testing <- period_2_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count2_testing<-period_2_testing |> group_by(permno) |> 
  filter(year == 1939, month == 1) |> 
  unique()

period_3_testing <- period_3_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count3_testing<-period_3_testing |> group_by(permno) |> 
  filter(year == 1943, month == 1) |> 
  unique()

period_4_testing <- period_4_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count4_testing<-period_4_testing |> group_by(permno) |> 
  filter(year == 1947, month == 1) |> 
  unique()

period_5_testing <- period_5_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count5_testing<-period_5_testing |> group_by(permno) |> 
  filter(year == 1951, month == 1) |> 
  unique()

period_6_testing <- period_6_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count6_testing<-period_6_testing |> group_by(permno) |> 
  filter(year == 1955, month == 1) |> 
  unique()

period_7_testing <- period_7_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count7_testing<-period_7_testing |> group_by(permno) |> 
  filter(year == 1959, month == 1) |> 
  unique()

period_8_testing <- period_8_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count8_testing<-period_8_testing |> group_by(permno) |> 
  filter(year == 1963, month == 1) |> 
  unique()

period_9_testing <- period_9_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count9_testing<-period_9_testing |> group_by(permno) |> 
  filter(year == 1967, month == 1) |> 
  unique()

period_10_testing <- period_10_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count10_testing<-period_10_testing |> group_by(permno) |> 
  filter(year == 1971, month == 1) |> 
  unique()

period_11_testing <- period_11_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count11_testing<-period_11_testing |> group_by(permno) |> 
  filter(year == 1975, month == 1) |> 
  unique()

period_12_testing <- period_12_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count12_testing<-period_12_testing |> group_by(permno) |> 
  filter(year == 1979, month == 1) |> 
  unique()

period_13_testing <- period_13_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count13_testing<-period_13_testing |> group_by(permno) |> 
  filter(year == 1983, month == 1) |> 
  unique()

period_14_testing <- period_14_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count14_testing<-period_14_testing |> group_by(permno) |> 
  filter(year == 1987, month == 1) |> 
  unique()

period_15_testing <- period_15_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count15_testing<-period_15_testing |> group_by(permno) |> 
  filter(year == 1991, month == 1) |> 
  unique()

period_16_testing <- period_16_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count16_testing<-period_16_testing |> group_by(permno) |> 
  filter(year == 1995, month == 1) |> 
  unique()

period_17_testing <- period_17_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count17_testing<-period_17_testing |> group_by(permno) |> 
  filter(year == 1999, month == 1) |> 
  unique()

period_18_testing <- period_18_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count18_testing<-period_18_testing |> group_by(permno) |> 
  filter(year == 2003, month == 1) |> 
  unique()

period_19_testing <- period_19_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count19_testing<-period_19_testing |> group_by(permno) |> 
  filter(year == 2007, month == 1) |> 
  unique()

period_20_testing <- period_20_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count20_testing<-period_20_testing |> group_by(permno) |> 
  filter(year == 2011, month == 1) |> 
  unique()

period_21_testing <- period_21_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count21_testing<-period_21_testing |> group_by(permno) |> 
  filter(year == 2015, month == 1) |> 
  unique()

period_22_testing <- period_22_testing |> 
  mutate(year = year(date),
         month = month(date)) 

stock_count22_testing<-period_22_testing |> group_by(permno) |> 
  filter(year == 2019, month == 1) |> 
  unique()





## number of securities meeting data requirement that is 5 years of data in estimation period (1930-34)
period_1_estimation <- period_1_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count1_estimation <- period_1_estimation %>%
    group_by(permno) |> 
    filter(n() == 60) |> 
  reframe(permno) |> 
    ungroup() |> 
  unique()


period_2_estimation <- period_2_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count2_estimation <- period_2_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_3_estimation <- period_3_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count3_estimation <- period_3_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_4_estimation <- period_4_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count4_estimation <- period_4_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_5_estimation <- period_5_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count5_estimation <- period_5_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_6_estimation <- period_6_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count6_estimation <- period_6_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_7_estimation <- period_7_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count7_estimation <- period_7_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_8_estimation <- period_8_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count8_estimation <- period_8_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_9_estimation <- period_9_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count9_estimation <- period_9_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_10_estimation <- period_10_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count10_estimation <- period_10_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_11_estimation <- period_11_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count11_estimation <- period_11_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_12_estimation <- period_12_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count12_estimation <- period_12_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_13_estimation <- period_13_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count13_estimation <- period_13_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_14_estimation <- period_14_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count14_estimation <- period_14_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_15_estimation <- period_15_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count15_estimation <- period_15_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_16_estimation <- period_16_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count16_estimation <- period_16_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_17_estimation <- period_17_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count17_estimation <- period_17_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_18_estimation <- period_18_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count18_estimation <- period_18_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_19_estimation <- period_19_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count19_estimation <- period_19_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_20_estimation <- period_20_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count20_estimation <- period_20_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_21_estimation <- period_21_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count21_estimation <- period_21_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_22_estimation <- period_22_estimation |> 
  mutate(year = year(date),
         month = month(date)) 

stocks_count22_estimation <- period_22_estimation %>%
  group_by(permno) |> 
  filter(n() == 60) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()










# find and count the stocks that have at least 4 years of observations in the portfolio formation period
period_1_formation <- period_1_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count1_formation <-  period_1_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_2_formation <- period_2_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count2_formation <-  period_2_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()

period_3_formation <- period_3_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count3_formation <-  period_3_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_4_formation <- period_4_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count4_formation <-  period_4_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_5_formation <- period_5_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count5_formation <-  period_5_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_6_formation <- period_6_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count6_formation <-  period_6_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_7_formation <- period_7_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count7_formation <-  period_7_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_8_formation <- period_8_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count8_formation <-  period_8_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_9_formation <- period_9_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count9_formation <-  period_9_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_10_formation <- period_10_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count10_formation <-  period_10_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_11_formation <- period_11_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count11_formation <-  period_11_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_12_formation <- period_12_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count12_formation <-  period_12_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_13_formation <- period_13_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count13_formation <-  period_13_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_14_formation <- period_14_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count14_formation <-  period_14_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_15_formation <- period_15_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count15_formation <-  period_15_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_16_formation <- period_16_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count16_formation <-  period_16_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_17_formation <- period_17_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count17_formation <-  period_17_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_18_formation <- period_18_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count18_formation <-  period_18_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_19_formation <- period_19_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count19_formation <-  period_19_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_20_formation <- period_20_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count20_formation <-  period_20_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_21_formation <- period_21_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count21_formation <-  period_21_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()


period_22_formation <- period_22_formation |> 
  mutate(year = year(date),
         month = month(date)) 
stocks_count22_formation <-  period_22_formation %>% 
  group_by(permno) |> 
  filter(n()>= 48) |> 
  reframe(permno) |> 
  ungroup() |> 
  unique()






###Matching estimation and formation period to get the total number of stocks as the final stocks for the data requirement
 Matched_stocks1<-inner_join(stocks_count1_estimation,stocks_count1_formation,by = join_by(permno)) |> 
   select(permno)

 Matched_stocks2<-inner_join(stocks_count2_estimation,stocks_count2_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks3<-inner_join(stocks_count3_estimation,stocks_count3_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks4<-inner_join(stocks_count4_estimation,stocks_count4_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks5<-inner_join(stocks_count5_estimation,stocks_count5_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks6<-inner_join(stocks_count6_estimation,stocks_count6_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks7<-inner_join(stocks_count7_estimation,stocks_count7_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks8<-inner_join(stocks_count8_estimation,stocks_count8_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks9<-inner_join(stocks_count9_estimation,stocks_count9_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks10<-inner_join(stocks_count10_estimation,stocks_count10_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks11<-inner_join(stocks_count11_estimation,stocks_count11_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks12<-inner_join(stocks_count12_estimation,stocks_count12_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks13<-inner_join(stocks_count13_estimation,stocks_count13_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks14<-inner_join(stocks_count14_estimation,stocks_count14_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks15<-inner_join(stocks_count15_estimation,stocks_count15_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks16<-inner_join(stocks_count16_estimation,stocks_count16_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks17<-inner_join(stocks_count17_estimation,stocks_count17_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks18<-inner_join(stocks_count18_estimation,stocks_count18_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks19<-inner_join(stocks_count19_estimation,stocks_count19_formation,by = join_by(permno)) |> 
   select(permno)
 
 Matched_stocks20<-inner_join(stocks_count20_estimation,stocks_count20_formation,by = join_by(permno)) |> 
   select(permno)
 Matched_stocks21<-inner_join(stocks_count21_estimation,stocks_count21_formation,by = join_by(permno)) |> 
   select(permno)
 Matched_stocks22<-inner_join(stocks_count22_estimation,stocks_count22_formation,by = join_by(permno)) |> 
   select(permno)

  
 
