# Capacity-planning_Costs_Simulation
Generated Insights regarding Capacity planning, costs involved in stacking up CRS medication due to REMS program requirement 

Summary 

# Simulation Parameters 
initial_stock = 0 # initial stock 
crs_onset = 30 #Adverse Event onset duration in days
crs_prob = 0.3 #Adverse event occurrence probability/Drug safety profile
num_pats = 10000 # Expected total of patients in year across all sites
num_sites = 50  # Number of Treatment Sites
price_tocil = 1500 #price of medication/one dosage in US dollars
num_sim = 1000 # number of simulations

# Simulation Results
Average Patients per site:201
Mean Annual Drug consumption per site:120 doses
Avg cost of CRS dosage stocking annually per site:$180000
Avg Total Cost of CRS dosage stocking annually for all sites:$9 MM
Avg Annual Cost per patient:$896
Percentage Cost of  Treatment per patient:0.29%
