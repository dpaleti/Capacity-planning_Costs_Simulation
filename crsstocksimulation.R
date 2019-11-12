# Estimate Treatment Cost per Patient - Simulation of Random Patient arrivals and CRS Medication stocking 
# Assumptions:

# patients arriving in any day(infact any hour) has no impact on previous or future arrivals i.e. independent
# duration monitored is 3 weeks (or) 15 days 
# Patients are uniformly distributed across months 
# Administrative period before infusion is 10 days
# Treatment REMS Cost per Patient is $ 300K
###############################################################
#Parameters 
initial_stock = 0 # initial stock 
crs_onset = 30 #CRS onset duration in days
crs_prob = 0.3 #CRS occurance probability/safety profile
num_pats = 10000 # Expected total of patients in year across all sites
num_sites = 50  # Number of Certified sites
price_tocil = 1500 #price of tocilizumab/one dosage
num_sim = 1000 # number of simulations


#libraries required
library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)
require(gridExtra)

# initialize variables
newa ={}
newda ={}
newcns ={}
quant_optimal_inventory = c()
pats_vector = c()
quant_total_consumption = c()
n = 0

for (n in 1:num_sim){
  duration = 264 # 22 workings days/month * 12 
  #pats_monthlydist = c('20','15','15','10','10','8','8','5','5','2','1','1')
  #pats_monthlydist = c('30','20','15','5','5','5','5','5','5','2','1','1')
  pats_monthlydist = rep((100/12),12) 
  #randomly pick a site and get the patient distribution, sd = 25 assumed
  num_pats_persite = round(rnorm(1,round(num_pats/num_sites),25),0)
  # Patient distribution Vector across months for a random site
  pat_per_site_month = round(num_pats_persite*(as.numeric(pats_monthlydist)/100))
  # Patient arrivals in an hour assuming Poisson dist, with a mean(#of patients arriving per hour)
  p = {}
  s = {}
  mon = {}
  pat_mat1 = {}
  pat_mat = {}
  # building patient matrix per hour, pat_mat1 (12*22), 12 months * 22 working days in a month 
  for (m in 1:12) {    
    pois_mean = (pat_per_site_month[m])/(22*8)
    for (j in 1:22){  #days 
      for (i in 1:8){ #hours 
        p[i] = rpois(1,pois_mean)
      }
      s[j] = sum(p)
      pat_mat <- matrix(s, nrow = 1, byrow = TRUE)
    }
    pat_mat1 <- rbind(pat_mat1,pat_mat)
    mon[m] = sum(s) 
  }
  # matrix transposed to a vector
  arriv_vector <- c() 
  arriv_vector <- as.vector(t(pat_mat1))
  a = seq(1:duration)
  
  # Datastructures: Arrivals, CRS Occured(1-Yes, 0-No), Inventory, Dose Consumption
  
  dos_vector <- c() 
  crs_occur ={}
  inventory ={}
  consumption = {}
  stockback_vector = {}
  
  for (i in 1:length(arriv_vector)) { 
    crs_occur[i] = sum(rbinom(arriv_vector[i],1,crs_prob))
    stockback_vector[i] <- arriv_vector[i] - crs_occur[i]
  }
  
  # ORDER more situation vs Reserve stock; Stock as u go situation
  #10days administrative period, first stocking on 11th day
  inventory[1:10] = initial_stock
  
  # start stocking up(add) for any arrivals until onset of 1st patient
  for (i in 11:(10+crs_onset)) {
    inventory[i] = inventory[i-1] + 2*arriv_vector[i-10]
  }
  
  #stockback gives back so you dont have to order more
  #Order stock for any arrival minus stock unused
  for (i in (11+crs_onset):length(crs_occur)) {
    inventory[i] = inventory[i-1] - 2*stockback_vector[i-(10+crs_onset)] + 2*(arriv_vector)[i-10]
    
    #   Replenish every 2 months if stock falls below a threshold     
    #    threshold = 100
    #    if(i%%44 == 0){
    #      if (inventory[i] < threshold) {
    #        inventory[i] = initial_stock
    #      }
    #    }
  }
  
  pats_vector[n] = sum(arriv_vector)
  #quant_optimal_inventory[n] = initial_stock - (sign(tail(inventory, n=1))*abs(tail(inventory, n=1)))
  # cumulative consumption of medication
  consumption <- cumsum(2*crs_occur)
  quant_total_consumption[n] = tail(consumption, n=1)
  dos_vector ={}
  dos_vector <- inventory 
  
  # preparing dosaze vector data for plotting , i.e. appending necessary columns to create month facets on plot
  a1 = {}
  dos1 = {}
  df = data.frame(days = a, dosaze = dos_vector)
  df1 = data.frame(days = a, dosaze = p)
  # newa (patient arrival vector) stores data for all simulations
  a1 <- matrix(t(data.frame(arriv_vector)),nrow = 1, byrow = TRUE)
  newa <- rbind(newa,a1)
  # newda(dosaze vector) stores data for all simulations 
  dos1 <- matrix(t(data.frame(dos_vector)),nrow = 1, byrow = TRUE)
  newda <- rbind(newda,dos1)
  # newcons(consumption vector) stores data for all simulations 
  cons1 <- matrix(t(data.frame(consumption)),nrow = 1, byrow = TRUE)
  newcns <- rbind(newcns,cons1)
  
}
# building vectors to make month facets for ggplot
# days vector
vecr = {}
z = {}
for (mn in 1:n) {
  for (l in 1:264){
    z[l] = l
  }
  vecr <- c(vecr, z)
}
matrix(vecr)

#months vector
vecr2 = {}
z2 = {}
for (mn in 1:n) {
  for (k in 1:12){
    for (l in 1:22){
      z2[l] = k
    }
    vecr2 <- c(vecr2, z2)
  }
}

#Plotting Dosaze data
c <- matrix(seq(1:n))
newda <- cbind(newda,c)
newcns <- cbind(newcns,c)
# making wide dataset to clean long dataset 
# X gets appended when converting matrix to a dataframe
l <- gather(data.frame(newda),key,inventory_stockup,-X265)
lc <- gather(data.frame(newcns),key,cumulative_dose_consumption,-X265)
df <- arrange(l,X265)
dfc <- arrange(lc,X265)
days<- matrix(vecr)
month<- matrix(vecr2)
#consum <- matrix(consumption)


df1 <- cbind(df,days,month)
names(dfc1)
dfc1 <- cbind(dfc,days,month)
#X265 - simulation index
#key - day index
# dosaze - Actual dosaze
#days - days sequence
#month - month sequence

# PLOTTING
#aes(frame=month, cumulative = TRUE)
#plot1 <- ggplot(df1, aes(x = days, y = inventory_stockup,  col = factor(month))) +   geom_line(aes(frame=month, cumulative = TRUE)) + theme(legend.position="none")

hist <- data.frame(quant_total_consumption)
twosdhigh <- mean(hist$quant_total_consumption) + 2*sd(hist$quant_total_consumption)
twosdlow <- mean(hist$quant_total_consumption) - 2*sd(hist$quant_total_consumption)
p <- ggplot(hist, aes(x=quant_total_consumption)) + geom_histogram(aes(y=..density..),binwidth=0.5,color="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")
q <- p + geom_vline(aes(xintercept=mean(quant_total_consumption)),color=5, linetype=3, size=1) + geom_vline(aes(xintercept=twosdhigh),color=5, linetype=3, size=1) + geom_vline(aes(xintercept=twosdlow),color=5, linetype=3, size=1)
#p + geom_vline(aes(xintercept=mean(quant_total_consumption)),color=5, linetype=3, size=1)
plot1 <- ggplot(df1, aes(x = days, y = inventory_stockup,  col = factor(month))) +   geom_line() + theme(legend.position="none")
plot2 <- ggplot(dfc1, aes(x = days, y = cumulative_dose_consumption,  col = factor(month))) +   geom_line(aes(frame=month, cumulative = TRUE)) + theme(legend.position="none")
#grid.arrange(plot1, plot2, ncol=2)
#plot1
#ggplotly(q)

#ggplotly(plot2)


# Interpreting results:
# Quants Based on Simulations, finding max dosaze needed per month
# **Inventory depletion and dosaze consumption Rates closely follow the Patient arrivals(R) rates,
# Consumption rates can be found out.
# ** Inventory Depletion/dosaze consumption across months follow Patients montly distribution
# ** CRS occurance prob(R) decides the consumption rate 
# Time to onset doesn't matter  
# Quantify depletion and consumption

# with or without replinshment, 
# Initial costs for stocking changes, puts burden of replenishment facility
#consumptiton doesn't change, is proportional to CRS occurance prob 
persite_avg_patients <- round(mean(pats_vector))
tot_cost <- (50*price_tocil*avg_consum_year)/1000000
avg_consum_year <- round(mean(quant_total_consumption))
patient_cost <- round((price_tocil*avg_consum_year)/avg_no_patients_year)
persite_cost <- price_tocil*avg_consum_year
# Average quants over all simulations

print(paste0 ('Average Patients per site:', persite_avg_patients))
print(paste0 ('Mean Annual Tocilizumab consumption per site:', avg_consum_year))
print(paste0 ('Avg cost of CRS dosage stocking annually per site:','$', persite_cost))
print(paste0('Avg Total Cost of CRS Dosage annaully for all sites:','$',tot_cost,' MM'))
#print(paste0('Avg Initial CRS Dosaze supplies required per site:', round(mean(quant_optimal_inventory[n]))))
print(paste0('Avg Annual Cost per patient:', '$', round((price_tocil*avg_consum_year)/persite_avg_patients)))
print(paste0('Percentage Cost of Treatment REMS per patient:', (patient_cost/300000)*100,'%'))

# 1.start with zero inventory and ADD up as we go
# grade 3 and up , prob 0.25. 025. 0.25
# crs prob 0.6, overall 30 %
#hist
# 2. put all Input parameters in one place and show all output paramerts on the graph ?
#(see if you can create a UI to play around with the parameters)
# 

# if costs of making sure Sites are compliant is more than distributing it for all patients then do it
# with safety profile 



