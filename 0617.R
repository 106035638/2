rm(list=ls())
library(tidyquant)
library(timetk)

#1使用套件tidyquant, timetk，並讀入資料  https://github.com/swtzang/FinDB_2019/tree/master/data_wrangle_practice/tej_day_price_2017_2018.txt
stock_day_2_year<-read_tsv("C:/0617/2/data_wrangle_practice/tej_day_price_2017_2018.txt")
glimpse(stock_day_2_year)


#2選取欄位“證券代碼”, “簡稱”, “年月日”, “收盤價(元)”, “市值(百萬元)”, 並將名稱改為“id”, “name”, “date”, “price”, “cap”。
price_day_2_year <- stock_day_2_year %>%
                    rename(id     = 證券代碼,
                           name   = 簡稱,
                           date   = 年月日,
                           price  = `收盤價(元)`,
                           cap    = `市值(百萬元)`
                    ) 
                    
dim(price_day_2_year)
  
                
#3選取id, date, price, 並將id改為文字格式，date改為日期格式，並將資料格式改為寬資料。提示：使用spread()。
price_day_2_year_1 <- stock_day_2_year %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  ) %>% 
  mutate(id = as.character(id)) %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  select(id, date, price) %>% 
  spread(key = id, value = price) 

dim(price_day_2_year_1)


#4檢查含有NA的股票代碼及其NA的個數。
price_day_2_year_na <- price_day_2_year %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_2_year_na

price_day_2_year_na.1 <- price_day_2_year %>% 
  # last observation carried forward
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_2_year_na.1


#5將NA值以最近的股價取代。提示：使用na.locf()
price_day_2_year_clear <-  price_day_2_year_1 %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>%
  select(-c("2025", "6131"))
dim(price_day_2_year_clear)


#6刪除上題中仍含有NA值的股票, 並確認股票數量及筆數。
dim(price_day_2_year_clear)


#7將資料轉為xts(提示：可用tk_xts()), 計算日報酬率(以log計算)(提示：可用Return.calculate()), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。
ret_day_2_year <- price_day_2_year_clear %>% 
                  select(1:6) %>%
                  tk_xts(select = -date, date_var = date) %>% 
                  Return.calculate(method = "log") %>%
                  na.omit()
                  
dim(ret_day_2_year)

head(ret_day_2_year,5)


#8計算月報酬率(以log計算)(提示：可用Return.calculate()), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。
price_day_2_year.xts <- price_day_2_year_clear %>% 
                  select(1:6) %>%
                  tk_xts(select = -date, date_var = date) 

ret_mon_2_year.xts <- price_day_2_year.xts %>% 
  to.period(period = "months", 
            indexAt = "lastof", 
            OHLC= FALSE) %>% 
  Return.calculate(method = "log") %>%
  na.omit()


dim(ret_mon_2_year.xts)

head(ret_mon_2_year.xts,5)


#9找出2017及2018年年底市值最大的前20家公司代碼, 簡稱, 並修改資本額格式，計算每家公司市值佔20家總市值的百分比。提示：使用filter(), arrange(), slice(), sum()。
price_day_2_year_1<-read_tsv("C:/0617/2/data_wrangle_practice/tej_day_price_2017_2018.txt")

price_day_2_year<- stock_day_2_year %>%
  rename(id     = 證券代碼,
         name   = 簡稱,
         date   = 年月日,
         price  = `收盤價(元)`,
         cap    = `市值(百萬元)`
  )

price_day_2_year %>%
  select(id,name, date, cap) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  summarise(revenue= sum(cap) %>%
  view()            
  
  