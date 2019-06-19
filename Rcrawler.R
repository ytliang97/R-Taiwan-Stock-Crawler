#程式目的：擷取台灣證券交易所單一股票每日的交易，並整理成quantmod套件可使用的格式
#使用範例：getTWStockData("20120201", "20120801", "2891", 15:17)
# -                       開始月份    結束月份    股票代號 適當的暫停秒數
#產生變數：xts object containing: Open, High, Low, Close, Volume. There is not Adjusted value temporarily.
#產生檔案：TW2891.csv, TW2891.rdata
#程式步驟：
# - 1.建一個指定開始日期至結束日期的array
# -   a.將日期array切割成每四個月進行擷取
# - 2.呼叫ProcessWebStockData()批次處理擷取的Data，並且合併
# -   b.片段的日期進行資料獲取及處理
# -   c.合併以處理資料及新處理好的資料
# - 3.讀取指定股票在台灣證券交易所網頁的Data Table，進行整理
# -   d.指定股票的每月交易資料網頁用readHTMLTable讀出
# -   e.將資料整理成日期+四個價+成交量(成交股數)
# -   f.調整日期(民國->西元)
# -   回傳資料，重複以上步驟直到所需年分資料皆下載完成
# - 4.處理2012/2/29特殊情況
# 註解意義:
# - "#":要用到的、解釋此步驟的意義
# - "# - ":註解下一行或說明
# - "###":測試用程式碼，正式的不會用到

# library(XML)
# library(RCurl)
# library(lubridate) ## for year() function
# library(plyr) ## for __ply() and rbind.fill()

#變數解釋：
# - FDofM means First Day of a Month / 每月的第一天
# - firstMofS means First Month of a Season / 每季的第一天

ProcessWebStockData <- function(date,stocknumstr){
  # - d.
  ###url <- 'http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=html&date=20120201&stockNo=2891' #完整網址
  # - url <- paste('base','currentdate','stockNo')
  url <- paste('http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=html&date=',date,'&stockNo=',stocknumstr,sep="")
  cat(url);cat("\n")
  web <- NULL
  tryCatch({
    web <- getURL(url)
  },
  warning = function(msg){
    message("Original warning message:")
    message(paste0(msg,"\n"))
  },
  error = function(msg){
    message("Original error message:")
    message(paste0(msg,"\n"))
    message("可能因為擷取資料過快，被網站擋下，建議將網路斷掉重新連線，換取新IP, 並將等待時間延長\n")
  }
  )
  stocktable <- NULL
  tryCatch({
    stocktable <- readHTMLTable(web, header=FALSE, strinsAsFactors=FALSE, which=1)
  },
  warning = function(msg){
    message("Original warning message:")
    message(paste0(msg,"\n"))
  },
  error = function(msg){
    message("Original error message:")
    message(paste0(msg,"\n"))
    cat(web)
  }
  )
  
  
  # - e.
  colnames(stocktable) <- c("日期","成交股數","成交金額","開盤價","最高價","最低價","收盤價","漲跌價差","成交筆數")
  uniStock <- stocktable[c("日期","開盤價","最高價","最低價","收盤價","成交股數")]
  colnames(uniStock) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  
  # - f.
  uniStock$Date <- as.Date(uniStock$Date,"%Y/%m/%d")
  date <- as.Date(date, "%Y%m%d")
  year(uniStock$Date) <- year(date)
  
  return (data.frame(Date = uniStock$Date, Open = uniStock$Open, High = uniStock$High, Low = uniStock$Low, Close = uniStock$Close, Volume = uniStock$Volume,stringsAsFactors=FALSE))
}

processDate <- function(date){
  date <- gsub("-","",date)
  return (date)
}

# - 2.
getPeriodTWStockData <- function(startdate,enddate,stocknumstr){
  filename <- paste("TW",stocknumstr,".csv",sep="")
  uniStock <- NULL
  if(file.exists(filename)){
    uniStock <- read.csv(filename,row.names = NULL)
  }
  
  dateArray <- seq(startdate,enddate, by = "month")
  dateArray <- as.character(dateArray)
  
  dateArray <- aaply(dateArray, .margins = 1, .fun = processDate)
  date <- dateArray[5]
  # - b.
  m_uniStock <- adply(dateArray, .margins = 1, .fun = ProcessWebStockData, .id = NULL, stocknumstr)
  
  cat("\n")
  rowmsg <- paste("get", as.character(nrow(m_uniStock)), "row data.")
  cat(rowmsg);cat("\n");
  
  uniStock$Date <- as.character(uniStock$Date)
  m_uniStock$Date <- as.character(m_uniStock$Date)
  # - c.
  uniStock <- rbind(uniStock,m_uniStock) # - can't use rbind handle because of date object
  uniStock$Date <- as.Date(uniStock$Date, "%Y-%m-%d")
  write.csv(uniStock,filename,row.names = FALSE)
}


getTWStockData <- function(startdatestr,enddatestr,stocknumstr,waitrange = 15:17){
  #stocknumstr <- "2891"
  filename <- paste("TW",stocknumstr,".csv",sep="")
  if(file.exists(filename))
    file.remove(filename)
  # - 1.
  startdate <- as.Date(startdatestr,"%Y%m%d")
  enddate <- as.Date(enddatestr,"%Y%m%d")
  dateArray <- seq(startdate,enddate, by = "month")
  
  # - a.
  firstMofS <- seq(1,length(dateArray),by = 4)
  firstMofS[length(firstMofS)]
  for(i in seq(1,length(dateArray),by = 4)){
    ###print(i)
    currstartdate <- dateArray[i]
    ###class(currstartdate)
    print(currstartdate)
    if(i != firstMofS[length(firstMofS)]){
      currenddate <- dateArray[i+3]  
    }
    else{
      l <- length(dateArray)%%4
      if(l == 0) currenddate <- dateArray[i+3]
      else currenddate <- dateArray[i+l-1]
    }
    
    print(currenddate)
    # - 2.
    getPeriodTWStockData(currstartdate,currenddate,stocknumstr)
    wait <- sample(waitrange,1)
    waitmsg <- paste("wait for ", as.character(wait)," seconds")
    cat(waitmsg)
    cat("\n\n")
    Sys.sleep(wait)
  }
  
  # - 4.
  uniStock <- read.csv(filename,row.names = NULL, stringsAsFactors = FALSE)
  uniStock$Volume <- gsub("\\,", "", uniStock$Volume)
  uniStock$Volume <- as.numeric(uniStock$Volume)
  uniStock$Date <- as.Date(uniStock$Date,"%Y-%m-%d")
  date <- as.Date("2012-02-29")
  if(sum( is.na( uniStock$Date ) ) > 0){
    uniStock$Date[which(is.na(uniStock$Date))] <- date
  }
  #uniStock <- xts(uniStock, order.by=uniStock[,1])
  write.csv(uniStock,filename,row.names = FALSE)
  rdataPath <- paste("./TW", stocknumstr, ".rdata", sep="")
  if(file.exists(rdataPath))
    file.remove(rdataPath)
  uniStock <- xts(uniStock[,-1], order.by=uniStock[,1])
  assign(paste("TW",stocknumstr, sep=""),uniStock)
  save(list=paste("TW", stocknumstr, sep=""), file=rdataPath)
}



# rdataPath <- "./TW2891.rdata"
# rdataPath <- paste("./", "StockAnalysis_Data", ".rdata", sep="")
# load(rdataPath)
# save(TW2891, TW2891_yahoo, file=rdataPath)