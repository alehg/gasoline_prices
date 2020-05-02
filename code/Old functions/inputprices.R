inputprices <- function(da,dates,prod) {
  
  # This function creates daily prices for each gas station taking the 
  # last price reported as the current price
  #
  # INPUT
  # df = database with prices
  # dates = vector of daily dates from the period of study
  #
  #
  # OUTPUT
  # daf= balanced panel with daily prices
  #
  dfprods <- function(p,dates,da){
    da<- filter(da,product == p)
    maxdate<- function(d,df){

      df %>% 
        filter(date_time_change <= d) %>% #get all obs. before cutoff date
        mutate(cutoff_datetime = d) %>%   #create cutoff date variable
        group_by(code) %>%                #group by id
        slice(which.max(date_time_change))#only get obs. with latest date

  }

  daf <- map_dfr(dates,maxdate,da) #iterate and bind into dataset

  return(daf)
  }
  v<- map(prod,dfprods,dates,da)
}


