# This is my R code for dissertation, if you are viewing this as an examiner, 
# please run all the code and look at the final section for the data source of each table.  
setwd("your working directory")
require(AER)
require(car)
require(stargazer)
require(ggplot2)
require(lmtest)
require(dynlm)
library(olsrr)
stock_cs = read.csv("comscore.csv")
stock_rt = read.csv("rentrak.csv")
####cut cs data with the same length of rt####
stock_cs = subset(stock_cs,stock_cs$date<=20160129)
nrow(stock_cs)
nrow(stock_rt)
####exam the data####
thin_trading = function(name){
  zero = subset(name, name$RET == 0)
  print(zero)
  low_volumn = subset(name, name$VOL<90000)
  print(low_volumn)
}
thin_trading(stock_cs)
thin_trading(stock_rt)#both dont have sever thin trading problem, hence s-w method posible 
####separate time window####
#cs
est_period_cs = subset(stock_cs, stock_cs$date>=20140909 & stock_cs$date<20150904)

test_period_1_cs = subset(stock_cs, stock_cs$date>=20150929 & stock_cs$date<=20151001) #[-1,+1],3days

test_period_2_cs = subset(stock_cs, stock_cs$date>=20150923 & stock_cs$date<=20151007) #[-5,+5],11days

test_period_3_cs = subset(stock_cs, stock_cs$date>=20150929 & stock_cs$date<=20160129) #[-1,83],85days
#rt
est_period_rt = subset(stock_rt, stock_rt$date>=20140909 & stock_rt$date<20150904)

test_period_1_rt = subset(stock_rt, stock_rt$date>=20150929 & stock_rt$date<=20151001) #[-1,+1],3days

test_period_2_rt = subset(stock_rt, stock_rt$date>=20150923 & stock_rt$date<=20151007) #[-5,+5],11days

test_period_3_rt = subset(stock_rt, stock_rt$date>=20150929 & stock_rt$date<=20160129) #[-1,83],85days
#combined 1: outstanding: cs=38.953mil rt= 15.357mil; price: cs=43.15, rt=43.82
cs_weight_neg2=38.953*43.15/(38.953*43.15+15.357*43.82)
rt_weight_neg2=1-cs_weight_neg2
combine_1 = as.data.frame(
  cbind(
    stock_cs$date,
    stock_cs$RET*cs_weight_neg2+stock_rt$RET*rt_weight_neg2,
    stock_cs$sprtrn
  )
)
colnames(combine_1)=c("date","RET","sprtrn")

est_period_comb1=subset(combine_1, combine_1$date>=20140909 & combine_1$date<20150904)
test_period_1_comb1 = subset(combine_1, combine_1$date>=20150929 & combine_1$date<=20151001)

#combined 2: outstanding: cs=38.953mil rt= 15.357mil; price: cs=47.43, rt= 46.35
cs_weight_neg6=38.953*47.43/(38.953*47.43+15.357*46.35)
rt_weight_neg6=1-cs_weight_neg6
combine_2=as.data.frame(
  cbind(
    stock_cs$date,
    stock_cs$RET*cs_weight_neg6+stock_rt$RET*rt_weight_neg6,
    stock_cs$sprtrn
  )
)
colnames(combine_2)=c("date","RET","sprtrn")

est_period_comb2=subset(combine_2, combine_2$date>=20140909 & combine_2$date<20150904)
test_period_2_comb2 = subset(combine_2, combine_2$date>=20150923 & combine_2$date<=20151007)

#combined 3: same as combine 1 because the starting date is the same 
combine_3=combine_1
est_period_comb3=est_period_comb1
test_period_3_comb3=subset(combine_3, combine_3$date>=20150929 & combine_3$date<=20160129)
####vanilla regression####
reg_mkt_cs = lm(RET~sprtrn,data = est_period_cs)
reg_mkt_rt = lm(RET~sprtrn,data = est_period_rt)
reg_mkt_comb1 = lm(RET~sprtrn,data = est_period_comb1)
reg_mkt_comb2 = lm(RET~sprtrn,data = est_period_comb2)
reg_mkt_comb3 = reg_mkt_comb1#same data,same regression 
stargazer(reg_mkt_cs,reg_mkt_rt,reg_mkt_comb1,reg_mkt_comb2,reg_mkt_comb3,type = "text")

#compute test_period_1 model expectation
test_period_1_cs$mkt_model_exp = reg_mkt_cs$coefficients[1]+reg_mkt_cs$coefficients[2]*test_period_1_cs$sprtrn
test_period_1_rt$mkt_model_exp = reg_mkt_rt$coefficients[1]+reg_mkt_rt$coefficients[2]*test_period_1_rt$sprtrn
test_period_1_comb1$mkt_model_exp = reg_mkt_comb1$coefficients[1]+reg_mkt_comb1$coefficients[2]*test_period_1_comb1$sprtrn
#compute test_period_2 model expectation
test_period_2_cs$mkt_model_exp = reg_mkt_cs$coefficients[1]+reg_mkt_cs$coefficients[2]*test_period_2_cs$sprtrn
test_period_2_rt$mkt_model_exp = reg_mkt_rt$coefficients[1]+reg_mkt_rt$coefficients[2]*test_period_2_rt$sprtrn
test_period_2_comb2$mkt_model_exp = reg_mkt_comb2$coefficients[1]+reg_mkt_comb2$coefficients[2]*test_period_2_comb2$sprtrn
#compute test_period_3 model expectation
test_period_3_cs$mkt_model_exp = reg_mkt_cs$coefficients[1]+reg_mkt_cs$coefficients[2]*test_period_3_cs$sprtrn
test_period_3_rt$mkt_model_exp = reg_mkt_rt$coefficients[1]+reg_mkt_rt$coefficients[2]*test_period_3_rt$sprtrn
test_period_3_comb3$mkt_model_exp = reg_mkt_comb3$coefficients[1]+reg_mkt_comb3$coefficients[2]*test_period_3_comb3$sprtrn

#compute abnormal return 
test_period_1_cs$abnormal = test_period_1_cs$RET - test_period_1_cs$mkt_model_exp
test_period_1_rt$abnormal = test_period_1_rt$RET - test_period_1_rt$mkt_model_exp
test_period_1_comb1$abnormal = test_period_1_comb1$RET - test_period_1_comb1$mkt_model_exp

test_period_2_cs$abnormal = test_period_2_cs$RET - test_period_2_cs$mkt_model_exp
test_period_2_rt$abnormal = test_period_2_rt$RET - test_period_2_rt$mkt_model_exp
test_period_2_comb2$abnormal = test_period_2_comb2$RET - test_period_2_comb2$mkt_model_exp

test_period_3_cs$abnormal = test_period_3_cs$RET - test_period_3_cs$mkt_model_exp
test_period_3_rt$abnormal = test_period_3_rt$RET - test_period_3_rt$mkt_model_exp
test_period_3_comb3$abnormal = test_period_3_comb3$RET - test_period_3_comb3$mkt_model_exp

#CAR
car_1_cs=sum(test_period_1_cs$abnormal)
car_1_rt=sum(test_period_1_rt$abnormal)
car_1_comb1=sum(test_period_1_comb1$abnormal)
car_1 = cbind(car_1_cs,car_1_rt,car_1_comb1)

car_2_cs=sum(test_period_2_cs$abnormal)
car_2_rt=sum(test_period_2_rt$abnormal)
car_2_comb2=sum(test_period_2_comb2$abnormal)
car_2 = cbind(car_2_cs,car_2_rt,car_2_comb2)

car_3_cs=sum(test_period_3_cs$abnormal)
car_3_rt=sum(test_period_3_rt$abnormal)
car_3_comb3=sum(test_period_3_comb3$abnormal)
car_3 = cbind(car_3_cs,car_3_rt,car_3_comb3)

car = as.data.frame(rbind(car_1,car_2,car_3),row.names = c("test_period_1","test_period_2","test_period_3"))
colnames(car)=c("car_cs","car_rt","car_combined")

car

#use Patell z statistics

patell_inrow = function(abnormal_return,test_mkt_return,est_data,reg_residuals){
  obs = nrow(est_data)
  se = sqrt((1/(obs-2))*sum(reg_residuals^2))
  c = 1+1/obs+(test_mkt_return - mean(est_data$sprtrn))^2/(var(est_data$sprtrn))
  v = abnormal_return/(se*sqrt(c))
  return (v)
}

#time period 1
t_1_cs = sum(
  patell_inrow(
    test_period_1_cs$abnormal,
    test_period_1_cs$sprtrn,
    est_period_cs,
    reg_mkt_cs$residuals
  )
)/sqrt(nrow(test_period_1_cs))

t_1_rt = sum(
  patell_inrow(
    test_period_1_rt$abnormal,
    test_period_1_rt$sprtrn,
    est_period_rt,
    reg_mkt_rt$residuals
  )
)/sqrt(nrow(test_period_1_rt))

t_1_comb1 = sum(
  patell_inrow(
    test_period_1_comb1$abnormal,
    test_period_1_comb1$sprtrn,
    est_period_comb1,
    reg_mkt_comb1$residuals
  )
)/sqrt(nrow(test_period_1_comb1))

t_1=cbind(t_1_cs,t_1_rt,t_1_comb1)
#time period 2
t_2_cs = sum(
  patell_inrow(
    test_period_2_cs$abnormal,
    test_period_2_cs$sprtrn,
    est_period_cs,
    reg_mkt_cs$residuals
  )
)/sqrt(nrow(test_period_2_cs))

t_2_rt = sum(
  patell_inrow(
    test_period_2_rt$abnormal,
    test_period_2_rt$sprtrn,
    est_period_rt,
    reg_mkt_rt$residuals
  )
)/sqrt(nrow(test_period_2_rt))

t_2_comb2 = sum(
  patell_inrow(
    test_period_2_comb2$abnormal,
    test_period_2_comb2$sprtrn,
    est_period_comb2,
    reg_mkt_comb2$residuals
  )
)/sqrt(nrow(test_period_2_comb2))

t_2=cbind(t_2_cs,t_2_rt,t_2_comb2)
#time period 3
t_3_cs = sum(
  patell_inrow(
    test_period_3_cs$abnormal,
    test_period_3_cs$sprtrn,
    est_period_cs,
    reg_mkt_cs$residuals
  )
)/sqrt(nrow(test_period_3_cs))

t_3_rt = sum(
  patell_inrow(
    test_period_3_rt$abnormal,
    test_period_3_rt$sprtrn,
    est_period_rt,
    reg_mkt_rt$residuals
  )
)/sqrt(nrow(test_period_3_rt))

t_3_comb3 = sum(
  patell_inrow(
    test_period_3_comb3$abnormal,
    test_period_3_comb3$sprtrn,
    est_period_comb3,
    reg_mkt_comb3$residuals
  )
)/sqrt(nrow(test_period_3_comb3))

t_3=cbind(t_3_cs,t_3_rt,t_3_comb3)

total_t = as.data.frame(
  rbind(t_1,t_2,t_3),
  row.names=c("test_period_1","test_period_2","test_period_3")
)
colnames(total_t)=c("cs","rt","combined")
total_t
#p-val for student t test. 
p_val =as.data.frame(
  rbind(
    cbind(
      pt((total_t[1,1]),nrow(test_period_1_cs),lower.tail = TRUE),
      pt((total_t[1,2]),nrow(test_period_1_rt),lower.tail = FALSE),
      pt((total_t[1,3]),nrow(test_period_1_comb1),lower.tail = FALSE)
    ),
    cbind(
      pt((total_t[2,1]),nrow(test_period_2_cs),lower.tail = TRUE),
      pt((total_t[2,2]),nrow(test_period_2_rt),lower.tail = FALSE),
      pt((total_t[2,3]),nrow(test_period_2_comb2),lower.tail = FALSE)
    ),
    cbind(
      pt((total_t[3,1]),nrow(test_period_3_cs),lower.tail = TRUE),
      pt((total_t[3,2]),nrow(test_period_3_rt),lower.tail = FALSE),
      pt((total_t[3,3]),nrow(test_period_3_comb3),lower.tail = FALSE)
    )
  ),row.names = c("test_period_1","test_period_2","test_period_3")
)
colnames(p_val) = c("CS","RT","Combined")
p_val



######adjust for thin trading in Scholes and Williams######

#3 regressions 
#normal t
scholes_williams = function(est_period,reg_mkt){
  temp = est_period[-1,]
  reg_t = lm(RET~sprtrn,data = temp)
  beta_t = reg_mkt$coefficients[2]
  #t-1
  lag_1 = as.data.frame(est_period$RET)
  lag_1 = lag_1[-1,]#remove first row of stock return
  lag_1 = cbind(lag_1,head(est_period$sprtrn,-1))#remove last row of market return
  lag_1 = as.data.frame(lag_1)
  colnames(lag_1)=c("RET","sprtrn")
  reg_lag_1 = lm(RET~sprtrn,data = lag_1)
  beta_tneg1 = reg_lag_1$coefficients[2]
  #t+1 
  lead_1 = as.data.frame(est_period$sprtrn)#get market return
  lead_1 = lead_1[-1,]#remove first market return
  lead_1 = cbind(lead_1,head(est_period$RET,-1))#remove last row of stock return
  lead_1 = as.data.frame(lead_1)
  colnames(lead_1) = c("sprtrn","RET")
  reg_prior_1 = lm(RET~sprtrn,data = lead_1)
  beta_tplus1 = reg_prior_1$coefficients[2]
  #auto-correlation coefficient of the market return rhom 
  rhom=cor(temp$sprtrn,lag_1$sprtrn)
  #beta_sw
  beta_sw=sum(beta_t,beta_tneg1,beta_tplus1)/(1+2*rhom)
  #alpha_sw 
  alpha_1 = (1/(nrow(temp)-1))*sum(temp$RET[2:(nrow(temp)-1)])
  alpha_2 = beta_sw*sum(temp$sprtrn[2:(nrow(temp)-1)])*(1/(nrow(temp)-1))
  alpha_sw = alpha_1-alpha_2
  coef_sw=cbind(alpha_sw,beta_sw)
  return (coef_sw)
}

cs_coef_sw=scholes_williams(est_period_cs,reg_mkt_cs)
rt_coef_sw=scholes_williams(est_period_rt,reg_mkt_rt)
comb_1_sw=scholes_williams(est_period_comb1,reg_mkt_comb1)
comb_2_sw=scholes_williams(est_period_comb2,reg_mkt_comb2)
comb_3_sw=scholes_williams(est_period_comb3,reg_mkt_comb3)
#compute expected return
test_period_1_cs$mkt_model_sw = cs_coef_sw[1]+cs_coef_sw[2]*test_period_1_cs$sprtrn
test_period_2_cs$mkt_model_sw = cs_coef_sw[1]+cs_coef_sw[2]*test_period_2_cs$sprtrn
test_period_3_cs$mkt_model_sw = cs_coef_sw[1]+cs_coef_sw[2]*test_period_3_cs$sprtrn

test_period_1_rt$mkt_model_sw = rt_coef_sw[1]+rt_coef_sw[2]*test_period_1_rt$sprtrn
test_period_2_rt$mkt_model_sw = rt_coef_sw[1]+rt_coef_sw[2]*test_period_2_rt$sprtrn
test_period_3_rt$mkt_model_sw = rt_coef_sw[1]+rt_coef_sw[2]*test_period_3_rt$sprtrn

test_period_1_comb1$mkt_model_sw = comb_1_sw[1]+comb_1_sw[2]*test_period_1_comb1$sprtrn
test_period_2_comb2$mkt_model_sw = comb_2_sw[1]+comb_2_sw[2]*test_period_2_comb2$sprtrn
test_period_3_comb3$mkt_model_sw = comb_3_sw[1]+comb_3_sw[2]*test_period_3_comb3$sprtrn
#abnormal return
test_period_1_cs$abnormal_sw = test_period_1_cs$RET-test_period_1_cs$mkt_model_sw
test_period_2_cs$abnormal_sw = test_period_2_cs$RET-test_period_2_cs$mkt_model_sw
test_period_3_cs$abnormal_sw = test_period_3_cs$RET-test_period_3_cs$mkt_model_sw

test_period_1_rt$abnormal_sw = test_period_1_rt$RET-test_period_1_rt$mkt_model_sw
test_period_2_rt$abnormal_sw = test_period_2_rt$RET-test_period_2_rt$mkt_model_sw
test_period_3_rt$abnormal_sw = test_period_3_rt$RET-test_period_3_rt$mkt_model_sw

test_period_1_comb1$abnormal_sw = test_period_1_comb1$RET-test_period_1_comb1$mkt_model_sw
test_period_2_comb2$abnormal_sw = test_period_2_comb2$RET-test_period_2_comb2$mkt_model_sw
test_period_3_comb3$abnormal_sw = test_period_3_comb3$RET-test_period_3_comb3$mkt_model_sw

#CAR_sw
car_1_cs_sw=sum(test_period_1_cs$abnormal_sw)
car_1_rt_sw=sum(test_period_1_rt$abnormal_sw)
car_1_comb1_sw=sum(test_period_1_comb1$abnormal_sw)
car_1_sw = cbind(car_1_cs_sw,car_1_rt_sw,car_1_comb1_sw)

car_2_cs_sw=sum(test_period_2_cs$abnormal_sw)
car_2_rt_sw=sum(test_period_2_rt$abnormal_sw)
car_2_comb2_sw=sum(test_period_2_comb2$abnormal_sw)
car_2_sw = cbind(car_2_cs_sw,car_2_rt_sw,car_2_comb2_sw)

car_3_cs_sw=sum(test_period_3_cs$abnormal_sw)
car_3_rt_sw=sum(test_period_3_rt$abnormal_sw)
car_3_comb3_sw=sum(test_period_3_comb3$abnormal_sw)
car_3_sw = cbind(car_3_cs_sw,car_3_rt_sw,car_3_comb3_sw)

car_sw = as.data.frame(rbind(car_1_sw,car_2_sw,car_3_comb3_sw),
                       row.names = c("test_period_1","test_period_2","test_period_3")
                       )
colnames(car_sw)=c("car_RT_sw","car_RT_sw","car_Combined_sw")

car_sw
#regression residuals for sw method need to be computed by hand 
est_period_cs$residuals = est_period_cs$RET-cs_coef_sw[1]-cs_coef_sw[2]*est_period_cs$sprtrn
est_period_rt$residuals = est_period_rt$RET-rt_coef_sw[1]-rt_coef_sw[2]*est_period_rt$sprtrn
est_period_comb1$residuals = est_period_comb1$RET-comb_1_sw[1]-comb_1_sw[2]*est_period_comb1$sprtrn
est_period_comb2$residuals = est_period_comb2$RET-comb_2_sw[1]-comb_2_sw[2]*est_period_comb2$sprtrn
est_period_comb3$residuals = est_period_comb3$RET-comb_3_sw[1]-comb_3_sw[2]*est_period_comb3$sprtrn
#t test with Patell 
#time period 1
t_1_cs_sw = sum(
  patell_inrow(
    test_period_1_cs$abnormal_sw,
    test_period_1_cs$sprtrn,
    est_period_cs,
    est_period_cs$residuals
  )
)/sqrt(nrow(test_period_1_cs))

t_1_rt_sw = sum(
  patell_inrow(
    test_period_1_rt$abnormal_sw,
    test_period_1_rt$sprtrn,
    est_period_rt,
    est_period_rt$residuals
  )
)/sqrt(nrow(test_period_1_rt))

t_1_comb1_sw = sum(
  patell_inrow(
    test_period_1_comb1$abnormal_sw,
    test_period_1_comb1$sprtrn,
    est_period_comb1,
    est_period_comb1$residuals
  )
)/sqrt(nrow(test_period_1_comb1))

t_1_sw=cbind(t_1_cs_sw,t_1_rt_sw,t_1_comb1_sw)


#time period 2
t_2_cs_sw = sum(
  patell_inrow(
    test_period_2_cs$abnormal_sw,
    test_period_2_cs$sprtrn,
    est_period_cs,
    est_period_cs$residuals
  )
)/sqrt(nrow(test_period_2_cs))

t_2_rt_sw = sum(
  patell_inrow(
    test_period_2_rt$abnormal_sw,
    test_period_2_rt$sprtrn,
    est_period_rt,
    est_period_rt$residuals
  )
)/sqrt(nrow(test_period_2_rt))

t_2_comb2_sw = sum(
  patell_inrow(
    test_period_2_comb2$abnormal_sw,
    test_period_2_comb2$sprtrn,
    est_period_comb2,
    est_period_comb2$residuals
  )
)/sqrt(nrow(test_period_2_comb2))

t_2_sw=cbind(t_2_cs_sw,t_2_rt_sw,t_2_comb2_sw)

#time period 3
t_3_cs_sw = sum(
  patell_inrow(
    test_period_3_cs$abnormal_sw,
    test_period_3_cs$sprtrn,
    est_period_cs,
    est_period_cs$residuals
  )
)/sqrt(nrow(test_period_3_cs))

t_3_rt_sw = sum(
  patell_inrow(
    test_period_3_rt$abnormal_sw,
    test_period_3_rt$sprtrn,
    est_period_rt,
    est_period_rt$residuals
  )
)/sqrt(nrow(test_period_3_rt))

t_3_comb3_sw = sum(
  patell_inrow(
    test_period_3_comb3$abnormal_sw,
    test_period_3_comb3$sprtrn,
    est_period_comb3,
    est_period_comb3$residuals
  )
)/sqrt(nrow(test_period_3_comb3))

t_3_sw=cbind(t_3_cs_sw,t_3_rt_sw,t_3_comb3_sw)

total_t_sw = as.data.frame(
  rbind(t_1_sw,t_2_sw,t_3_sw),
  row.names=c("test_period_1","test_period_2","test_period_3")
)
colnames(total_t_sw)=c("CS","RT","Combined")
total_t_sw
#p-val for student t test. 
p_val_sw =as.data.frame(
  rbind(
    cbind(
      pt((total_t_sw[1,1]),nrow(test_period_1_cs),lower.tail = TRUE),
      pt((total_t_sw[1,2]),nrow(test_period_1_rt),lower.tail = FALSE),
      pt((total_t_sw[1,3]),nrow(test_period_1_comb1),lower.tail = FALSE)
    ),
    cbind(
      pt((total_t_sw[2,1]),nrow(test_period_2_cs),lower.tail = TRUE),
      pt((total_t_sw[2,2]),nrow(test_period_2_rt),lower.tail = FALSE),
      pt((total_t_sw[2,3]),nrow(test_period_2_comb2),lower.tail = FALSE)
    ),
    cbind(
      pt(-abs(total_t_sw[3,1]),nrow(test_period_3_cs),lower.tail = TRUE),
      pt(-abs(total_t_sw[3,2]),nrow(test_period_3_rt),lower.tail = FALSE),
      pt((total_t_sw[3,3]),nrow(test_period_3_comb3),lower.tail = FALSE)
    )
  ),row.names = c("test_period_1","test_period_2","test_period_3")
)
colnames(p_val_sw) = c("CS","RT","Combined")
p_val_sw



####sentiment analysis reg####
sentiment_cs = read.csv("day_sentiment_search_cs.csv")
sentiment_cs_mg = read.csv("day_sentiment_merger_cs.csv")
sentiment_rt = read.csv("day_sentiment_search_rt.csv")
sentiment_rt_mg = read.csv("day_sentiment_merger_rt.csv")
#the twitter data is daily, but market dont trade on holiday and weekend. 
#also the news happen at -1, after the market closes, so will have to attribute 
#twitter -> AR
#20150929->20150930
#20150928->20150929

row_add = function(df,date,direction){
  df_index=which(df$Time == date)# assume time is unique and date is correct
  if (is.na(df[df_index,][1,2])) {
    up=df[1:(df_index-1),]
    down=df[(df_index+1):nrow(df),]
    res=as.data.frame(rbind(up,down))
    colnames(res)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
    row.names(res)<-NULL
    return (res)
  }
  if (direction=="up") {
    temp=df[(df_index-1):df_index,]
    up=df[1:(df_index-2),]
    down = df[(df_index+1):nrow(df),]
    date = df[(df_index-1),]$Time
  }
  if(direction == "down"){
    temp=df[df_index:(df_index+1),]
    up=df[1:(df_index-1),]
    down=df[(df_index+2):nrow(df),]
    date = df[(df_index+1),]$Time
  }
  print(temp)
  sumed =as.data.frame(
    cbind(
    date,
    sum(temp[1,2],temp[2,2]),
    sum(temp[1,3],temp[2,3]),
    0,
    sum(temp[1,5],temp[2,5])
    )
  ) 
  colnames(sumed)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
  sumed[1,4]=sumed[1,3]/sumed[1,2]
  res=as.data.frame(
    rbind(up,sumed,down)
  )
  colnames(res)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
  row.names(res)<-NULL
  return (res)
}


split_add = function(df,mid_date){
  df_index = which(df$Time==mid_date)
  if (is.na(df[df_index,][1,2])) {
    up=df[1:(df_index-1),]
    down=df[(df_index+1):nrow(df),]
    res=as.data.frame(rbind(up,down))
    colnames(res)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
    row.names(res)<-NULL
    return (res)
  }
  temp = df[df_index,]
  up = df[1:(df_index-1),]
  down = df[(df_index+1):nrow(df),]
  mid = as.data.frame(
    rbind(
      cbind(1,temp[1,2]/2,temp[1,3]/2,0,temp[1,5]/2),
      cbind(2,temp[1,2]/2,temp[1,3]/2,0,temp[1,5]/2)
    )
  )
  colnames(mid)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
  new_df=as.data.frame(rbind(up,mid,down))
  colnames(new_df)=c("Time","num_of_tweets","num_of_positive","pct","num_of_negative")
  row.names(new_df)=NULL
  new_df=row_add(new_df,1,"up")
  new_df=row_add(new_df,2,"down")
  return (new_df)
}

adjust_trading = function(sentiment_df){
  
  #adjust the start 
  sentiment_df = row_add(sentiment_df,20150929,"down")
  
  #adjust weekends
  sentiment_df = row_add(sentiment_df,20151003,"up")
  sentiment_df = row_add(sentiment_df,20151004,"down")
  
  sentiment_df = row_add(sentiment_df,20151010,"up")
  sentiment_df = row_add(sentiment_df,20151011,"down")
  
  sentiment_df = row_add(sentiment_df,20151017,"up")
  sentiment_df = row_add(sentiment_df,20151018,"down")
  
  sentiment_df = row_add(sentiment_df,20151024,"up")
  sentiment_df = row_add(sentiment_df,20151025,"down")
  
  sentiment_df = row_add(sentiment_df,20151031,"up")
  sentiment_df = row_add(sentiment_df,20151101,"down")
  
  sentiment_df = row_add(sentiment_df,20151107,"up")
  sentiment_df = row_add(sentiment_df,20151108,"down")
  
  sentiment_df = row_add(sentiment_df,20151114,"up")
  sentiment_df = row_add(sentiment_df,20151115,"down")
  
  sentiment_df = row_add(sentiment_df,20151121,"up")
  sentiment_df = row_add(sentiment_df,20151122,"down")
  #2015/11/26 no trading 
  sentiment_df = split_add(sentiment_df,20151126)
  
  sentiment_df = row_add(sentiment_df,20151128,"up")
  sentiment_df = row_add(sentiment_df,20151129,"down")
  
  sentiment_df = row_add(sentiment_df,20151205,"up")
  sentiment_df = row_add(sentiment_df,20151206,"down")
  
  sentiment_df = row_add(sentiment_df,20151212,"up")
  sentiment_df = row_add(sentiment_df,20151213,"down")
  
  sentiment_df = row_add(sentiment_df,20151219,"up")
  sentiment_df = row_add(sentiment_df,20151220,"down")
  
  # Christmas, no trading for 12/25 12/26 12/27
  sentiment_df = row_add(sentiment_df,20151225,"up")
  sentiment_df = row_add(sentiment_df,20151227,"down")
  sentiment_df = split_add(sentiment_df,20151226)
  # end of Christmas
  # new year, no trading for 1/1, 1/2, 1/3
  sentiment_df = row_add(sentiment_df,20160101,"up")
  sentiment_df = row_add(sentiment_df,20160103,"down")
  sentiment_df = split_add(sentiment_df,20160102)
  #end of newyear
  # weekend
  sentiment_df = row_add(sentiment_df,20160109,"up")
  sentiment_df = row_add(sentiment_df,20160110,"down")
  # Martin Luther King, no trading 1/16,1/17,1/18
  sentiment_df = row_add(sentiment_df,20160116,"up")
  sentiment_df = split_add(sentiment_df,20160117)
  sentiment_df = row_add(sentiment_df,20160118,"down")
  # RIP Martin
  
  sentiment_df = row_add(sentiment_df,20160123,"up")
  sentiment_df = row_add(sentiment_df,20160124,"down")
  #end of trading day correction
  #adjust first date to 9/29
  sentiment_df$Time[1]="20150929"
  return (sentiment_df)
}

#get a slice of time column
std_time=as.data.frame(sentiment_cs$Time)
colnames(std_time)=c("Time")
#sentiment CS adjust
sentiment_cs=adjust_trading(sentiment_cs)
#unify the time column to avoide errors
sentiment_cs_mg = merge(std_time,sentiment_cs_mg,by="Time",all=TRUE)
sentiment_rt = merge(std_time,sentiment_rt,by="Time",all = TRUE)
sentiment_rt_mg=merge(std_time,sentiment_rt_mg,by="Time",all=TRUE)

sentiment_cs_mg=adjust_trading(sentiment_cs_mg)
sentiment_rt=adjust_trading(sentiment_rt)
sentiment_rt_mg=adjust_trading(sentiment_rt_mg)

colnames(sentiment_cs)=c("date","cs_num_tweets","cs_num_positive","cs_pct","cs_num_neg")
colnames(sentiment_cs_mg)=c("date","csmg_num_tweets","csmg_num_positive","csmg_pct","csmg_num_neg")
colnames(sentiment_rt)=c("date","rt_num_tweets","rt_num_positive","rt_pct","rt_num_neg")
colnames(sentiment_rt_mg)=c("date","rtmg_num_tweets","rtmg_num_positive","rtmg_pct","rtmg_num_neg")



####Regression using sentiment####
#first merge the sentiment data with test_period dataframe

#period 3 cs
test_period_3_cs=merge(test_period_3_cs,sentiment_cs,by = "date",all=TRUE)
test_period_3_cs=merge(test_period_3_cs,sentiment_cs_mg,by="date",all=TRUE)
test_period_3_cs=merge(test_period_3_cs,sentiment_rt_mg,by="date",all=TRUE)
test_period_3_cs=merge(test_period_3_cs,sentiment_rt,by="date",all=TRUE)

test_period_3_rt=merge(test_period_3_rt,sentiment_rt,by="date",all=TRUE)
test_period_3_rt=merge(test_period_3_rt,sentiment_rt_mg,by="date",all=TRUE)
test_period_3_rt=merge(test_period_3_rt,sentiment_cs,by="date",all=TRUE)
test_period_3_rt=merge(test_period_3_rt,sentiment_cs_mg,by="date",all=TRUE)


test_period_3_comb3=merge(test_period_3_comb3,sentiment_cs_mg,by="date",all=TRUE)
test_period_3_comb3=merge(test_period_3_comb3,sentiment_cs,by="date",all=TRUE)
test_period_3_comb3=merge(test_period_3_comb3,sentiment_rt,by="date",all=TRUE)
test_period_3_comb3=merge(test_period_3_comb3,sentiment_rt_mg,by="date",all=TRUE)

####base regression + best subset to find the best combination####

reg_sentiment_cs_0 = lm(abnormal_sw~0+csmg_pct+rtmg_pct+rtmg_num_positive+
                          rtmg_num_neg+csmg_num_positive+csmg_num_neg+
                          cs_pct+rt_pct+rt_num_positive+
                          rt_num_neg+cs_num_positive+cs_num_neg,
                        data = test_period_3_cs,
                        na.action = na.omit)

ols_step_best_subset(reg_sentiment_cs_0)#rtmg_num_neg rt_pct
#best regression for cs
best_sentiment_cs = lm(abnormal_sw~0+rtmg_num_neg+rt_pct,data = test_period_3_cs,
                       na.action = na.omit)
ols_regress(best_sentiment_cs)

reg_sentiment_rt_0 = lm(abnormal_sw~0+csmg_pct+rtmg_pct+rtmg_num_positive+
                          rtmg_num_neg+csmg_num_positive+csmg_num_neg+
                          cs_pct+rt_pct+rt_num_positive+
                          rt_num_neg+cs_num_positive+cs_num_neg,
                        data = test_period_3_rt,
                        na.action = na.omit)

ols_step_best_subset(reg_sentiment_rt_0)#rtmg_pct cs_pct rt_pct rt_num_neg

#best regression for rt 
best_sentiment_rt = lm(abnormal_sw~0+rtmg_pct +cs_pct+ rt_pct+ rt_num_neg, 
                       data=test_period_3_rt, na.action = na.omit)

ols_regress(best_sentiment_rt)

reg_sentiment_comb_0 = lm(abnormal_sw~0+csmg_pct+rtmg_pct+rtmg_num_positive+
                            rtmg_num_neg+csmg_num_positive+csmg_num_neg+
                            cs_pct+rt_pct+rt_num_positive+
                            rt_num_neg+cs_num_positive+cs_num_neg,
                          data=test_period_3_comb3,na.action = na.omit)

ols_step_best_subset(reg_sentiment_comb_0)#rtmg_num_neg rt_pct 

best_sentiment_comb = lm(abnormal_sw~0+rtmg_num_neg+rt_pct, data = test_period_3_comb3,
                         na.action = na.omit)
ols_regress(best_sentiment_comb)

####Hetroskedasticity test####

#if reject then hetro present
bptest(best_sentiment_cs)#don't reject, no Hetroskedasticity 
bptest(best_sentiment_rt)#reject, present
bptest(best_sentiment_comb)#don't reject, no Hetroskedasticity 

#adjust for Hetroskedasticity in the last model
coeftest(best_sentiment_rt,vcov. = vcovHC(best_sentiment_rt,"HC1"))#HC1 is the white standard errors

####Ramsy RESET test####
resettest(best_sentiment_cs)
resettest(best_sentiment_rt)
resettest(best_sentiment_comb) #all pass
####auto correlation test####
bgtest(best_sentiment_cs,order = 5)
bgtest(best_sentiment_rt,order = 5)
bgtest(best_sentiment_comb,order = 5) #all pass

###store the test_period in csv###
# write.csv(test_period_1_comb1,"st_result\\test_period_1_comb1.csv", row.names = FALSE)
# write.csv(test_period_1_cs,"st_result\\test_period_1_cs.csv", row.names = FALSE)
# write.csv(test_period_1_rt,"st_result\\test_period_1_rt.csv", row.names = FALSE)
# 
# write.csv(test_period_2_comb1,"st_result\\test_period_2_comb2.csv", row.names = FALSE)
# write.csv(test_period_2_cs,"st_result\\test_period_2_cs.csv", row.names = FALSE)
# write.csv(test_period_2_rt,"st_result\\test_period_2_rt.csv", row.names = FALSE)
# 
# write.csv(test_period_3_comb3,"st_result\\test_period_3_comb3.csv", row.names = FALSE)
# write.csv(test_period_3_cs,"st_result\\test_period_3_cs.csv", row.names = FALSE)
# write.csv(test_period_3_rt,"st_result\\test_period_3_rt.csv", row.names = FALSE)


####for examiner to check the data#### 

# section 6.1 table 1 is purely calculated by hand. 
# section 6.2 table 2,3,4 are in python code
# section 7.1 table 5
stargazer(reg_mkt_cs,reg_mkt_rt,reg_mkt_comb1,reg_mkt_comb2,reg_mkt_comb3,title="Estimated Market Model",type = "text",covariate.labels = c("Beta Hat","Alpha Hat"))
print(rbind(cs_coef_sw,rt_coef_sw,comb_1_sw,comb_2_sw,comb_3_sw))
# section 7.1 table 6
car
car_sw
total_t
total_t_sw
p_val
p_val_sw

#section 7.2
rbind(
  cbind(
    sum(sentiment_cs$cs_num_tweets),
    sum(sentiment_rt$rt_num_tweets),
    sum(sentiment_cs_mg$csmg_num_tweets,na.rm = TRUE),
    sum(sentiment_rt_mg$rtmg_num_tweets,na.rm = TRUE)
  ),
  cbind(
    sum(sentiment_cs$cs_num_positive),
    sum(sentiment_rt$rt_num_positive),
    sum(sentiment_cs_mg$csmg_num_positive,na.rm = TRUE),
    sum(sentiment_rt_mg$rtmg_num_positive,na.rm = TRUE)
  ),
  cbind(
    sum(sentiment_cs$cs_num_neg),
    sum(sentiment_rt$rt_num_neg),
    sum(sentiment_cs_mg$csmg_num_neg,na.rm = TRUE),
    sum(sentiment_rt_mg$rtmg_num_neg,na.rm = TRUE)
  ),
  cbind(
    mean(sentiment_cs$cs_num_tweets),
    mean(sentiment_rt$rt_num_tweets),
    mean(sentiment_cs_mg$csmg_num_tweets,na.rm = TRUE),
    mean(sentiment_rt_mg$rtmg_num_tweets,na.rm = TRUE)
  ),
  cbind(
    mean(sentiment_cs$cs_num_positive),
    mean(sentiment_rt$rt_num_positive),
    mean(sentiment_cs_mg$csmg_num_positive,na.rm = TRUE),
    mean(sentiment_rt_mg$rtmg_num_positive,na.rm = TRUE)
  ),
  cbind(
    mean(sentiment_cs$cs_num_neg),
    mean(sentiment_rt$rt_num_neg),
    mean(sentiment_cs_mg$csmg_num_neg,na.rm = TRUE),
    mean(sentiment_rt_mg$rtmg_num_neg,na.rm = TRUE)
  )
)
#section 7.2 table 8
#this may take a while.
ols_step_best_subset(reg_sentiment_cs_0)
ols_step_best_subset(reg_sentiment_rt_0)
ols_step_best_subset(reg_sentiment_comb_0)
