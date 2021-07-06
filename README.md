# r-programing
##Q1 check data type of each column
#strucutre
df=data.frame(kc_house_data)
df
str(df)
##Q2 all attribute in num an doublle 

##Q3 display data range of each column
summary(df)
sapply(df,range)
## outlier
par(mfrow=c(1,1))
boxplot(df)
boxplot(df$date)
boxplot(df$price)         #
boxplot(df$bedrooms)      #
boxplot(df$bathrooms,ylab="bathrooms")     #
boxplot(df$sqft_living)   #
boxplot(df$sqft_lot)      #
boxplot(df$floors)
boxplot((df$waterfront))  #
boxplot(df$view)          #
boxplot(df$condition)     #
boxplot(df$grade)         #
boxplot(df$sqft_above)    #
boxplot(df$sqft_basement) #
boxplot(df$yr_built)
boxplot(df$yr_renovated)  #
boxplot(df$zipcode)
boxplot(df$lat)           #
boxplot(df$long)          #
boxplot(df$sqft_living15) #
boxplot(df$sqft_lot15)    #

a= df  ##variable assign
b= df$bedrooms
c= df$bathrooms
d= df$sqft_living
e= df$sqft_lot
f= df$waterfront
g= df$view
h= df$condition
i= df$grade
j= df$sqft_above
k= df$sqft_basement
l= df$yr_renovated
m= df$lat
n= df$long
o= df$sqft_living15
p= df$sqft_lot15

## removing outlier

aa <-boxplot.stats(a)$out   
bb <-boxplot.stats(b)$out
cc <-boxplot.stats(c)$out
dd <-boxplot.stats(d)$out
ee <-boxplot.stats(e)$out
ff <-boxplot.stats(f)$out
gg <-boxplot.stats(g)$out
hh <-boxplot.stats(h)$out
ii <-boxplot.stats(i)$out
jj <-boxplot.stats(j)$out
kk <-boxplot.stats(k)$out
ll <-boxplot.stats(l)$out
mm <-boxplot.stats(m)$out
nn <-boxplot.stats(n)$out
oo <-boxplot.stats(o)$out
pp <-boxplot.stats(p)$out


a_out <-which(a %in% c(aa)) #condation fun   
b_out <-which(b %in% c(bb))
c_out <-which(c %in% c(cc))
d_out <-which(d %in% c(dd))
e_out <-which(e %in% c(ee))
f_out <-which(f %in% c(ff))
g_out <-which(g %in% c(gg))
h_out <-which(h %in% c(hh))
i_out <-which(i %in% c(ii))
j_out <-which(j %in% c(jj))
k_out=which(k %in% c(kk))
l_out <-which(l %in% c(ll))
m_out <-which(m %in% c(mm))
n_out <-which(n %in% c(nn))
o_out <-which(o %in% c(oo))
p_out <-which(p %in% c(pp))

boxplot(a_out)
boxplot(b_out)
boxplot(c_out,ylab="bathroom")
boxplot(d_out)
boxplot(e_out)
boxplot(f_out)
boxplot(g_out)
boxplot(h_out)
boxplot(i_out)
boxplot(j_out)
boxplot(k_out)
boxplot(l_out)
boxplot(m_out)
boxplot(n_out)
boxplot(o_out)
boxplot(p_out)

##Q4 factor influance the flat price
par(mfrow=c(3,3))
plot(df$price,df$bedrooms)
plot(df$price,df$bathrooms)
plot(df$price,df$sqft_living)
plot(df$price,df$sqft_lot)
plot(df$price,df$floors)
plot(df$price,df$waterfront)
plot(df$price,df$bedrooms)
plot(df$price,df$bedrooms)


#Q.2> 

##Q1.plot is drawn for Squared Ft Living data vs. House Price and share observations

plot(df$sqft_living, df$price)
# result:- as the size of living per squared ft increase, the price of house increase.

##Q2####2  2.	What is popular bedrooms size for houses in zip code area 98001
library(dplyr)
y=filter(df, zipcode == "98001");y   #subset data frame
yy=y$bedrooms;yy
s=table(yy);s
barplot(s, main = "Popular bedrooms size",col = "red")
#the house having 3 bedrooms are more popular in area having zip code 98001


##Q2###3  3.	Display Average pricing of houses per bedrooms size in zip code area 98001
avg_price=mean(df$price[df$zipcode==98001]);avg_price
p11=tapply(y$price,y$bedrooms,mean)
p11

##Q4. Command for plotting stacked graph (stack of bedroom sizes)
#for number of flats per year built (years between 1978 to 2010)
#Share observation

x = subset(df,df$yr_built>=1978 & df$yr_built<=2010)

bed_vs_yr = table(x$bedrooms,x$yr_built)
bed_vs_yr

barplot(bed_vs_yr,main="stack of bedroom sizes", 
        xlab="years between 1978 to 2010",
        col=c("darkblue","darkgreen","green","red",
              "yellow","blue","black","orange",
              "white","pink","skyblue"), 
        legend = rownames(bed_vs_yr),beside = FALSE)

