# 1.Project Background 
# File owner: Weixin Huang
# Creation data: 20231020
# Version number: 20241021
# Update content: Correct estimate of Qdose = DCD*DDDA

# 2. Manual of this R file
# Using Trapp's model (2023) and Carmen's model to simulate the concentration of 8 antibiotics in roots, shoots, grains of maize, and in milk and meat of cattles in circular food production.
# Instruction: The constant AM dosage scenario is estimated following the flow of codes, while the codes for worst-case Dutch-based scenario is highlighted with "***worst-case***" 


# 3. Package loading
library(readxl)
library(openxlsx)

# 4. Import parameters: choose CowSOIL file to import data. This file has updated degradation values in soil
# 4.1a Cow Combined diet
Para_CowCombined <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Cow_combined.xlsx")
Para_CowCombined <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Cow_combined.xlsx")
Para <- Para_CowCombined[,4:11]

# 4.1b Cow silage only diet
Para_CowSilage <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Cow_silage.xlsx")
Para_CowSilage <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Cow_silage.xlsx")
Para <- Para_CowSilage[,4:11]

# 4.1c Veal calves diet
Para_Veal <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Veal.xlsx")
Para_Veal <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/Antibiotics/Antibiotics_parameters_Veal.xlsx")
Para <- Para_Veal[,4:11]

# 4.2 create matrix for parameters
Para <- as.matrix(Para)
str(Para)

# 5. Animal-manure-soil module parameters (Unit)
# 5.1 Animal-related parameters
DDDA <- Para[77,]                          # Defined daily dosed animal per year (kg treated animals/(day*kg animals)
Fexcreted <- Para[73,]                     # Fraction of active substance excreted in faces (%)
Manimal <- Para[74,]                       # Average body weight (kg/animal)
T1storage <- Para[76,]                     # Storage time of manure before fertilizing (day)
Ncyclus_anim <- Para[79,]                  # Number of life cycles of farmed animals per year
Npday <- Para[80,]                         # Nitrogen production per animal per day (kg/(animal*day))
F_silage <- Para[94,]                      # Remaining dry matter of maize silage after ensiling (%)
DM_silage <- Para[90,]                     # Dry matter content of maize silage (%)
DM_grain <- Para[91,]                      # Dry matter content of maize grain (%)
t_animal <- Para[96,]                      # Lifecycle of farmed animals (day)
Ratio_meat <- Para[97,]                    # Ratio of carcass to live weight (%)
Y_milk <- Para[98,]                        # Daily yield of milk (kg/day)
t_milking <- Para[99,]                     # Lactation period of farmed animals (day)

# 5.2 Compound-related parameters
DT50manure <-Para[81,]                     # Half-life of antimicrobial in manure (day)
DT50soil <- Para[82,]                      # Half-life of antimicrobial in soil (day)
DCD <- Para[101,]                          # Defined course dose of AMs (mg/kg treated animals)

# 5.3 Soil-related parameters
Nst <- Para[83,]                           # Nitrogen fertilization standard in EU (kg/ha)
DEPTHfield <- Para[84,]                    # Mixing depth of manure in soil (m)
Conv_area_factor <- Para[85,]              # Conversion factor for the area of agricultural field (m2/ha)
ρDry_soil <- Para[4,]                      # Density of dry soil (kg/L)
Vwater_soil <- Para[5,]                    # Volume fraction of water in soil (L/L)

# 6. Soil-crop module parameters.
# 6.1. Soil parameters
OC <- Para[2,]                             # Eq1: Fraction of organic carbon in soil (g/g)
pH_soil <- Para[7,]                        # Eq2&19: Soil pH
I_soil <- Para[57,]                        # Eq15&16: Ion strength in soil (mol/L)

# 6.2 Compound parameters
lgKow_n <- Para[6,]                        # Eq2&20&21: logarithm (to the base 10) of partition coefficient of chemicals (neutral status) from octanol to water (L/kg)
pKa <- Para[8,]                            # Eq2&19: Compound pKa
Kaw <- Para[35,]                           # Eq25&26&27: Partition coefficient from air to water, equals to dimensionless Henry's law constant of the compound (m^3 air/m^3 water)
Khsa <- Para[19,]                          # Eq15: Partition coefficient to human serum albumin (L/mol)
kdg <- Para[48,]                           # Compound degradation & metabolism rate in plants 
z <- Para[21,]                             # Eq15: Valency or charge number

# 6.3 Root parameters
Q_root <- Para[10,]                        # Eq8: Transpiration stream from roots (total transpiration) (L/day)
RatioM_root <- Para[11,]                   # Adjusted: Proportion of root biomass to a total plant (%)
k_root <- Para[12,]                        # Eq8: Rate of overall loss due to dilution by growth and metabolism
Vwater_root <- Para[13,]                   # Eq9: Volumetic water content of roots (L/L)
Lipid_root <- Para[14,]                    # Eq9&20&21: Lipid content of roots (g/g root fresh weight)
Pro_root <- Para[26,]                      # Eq20&21: Protein content of the roots (g/g root fresh weight)

# 6.4 Leave parameters
RatioQ_leaf <- Para[27,]                   # Adjusted: Ratio of transpiration stream from leaves to the total/root (L/day)
g <- Para[29,]                             # Eq25&26&27&28: Conductivity of leaves (m/day)
area_leaf <- Para[30,]                     # Eq25&26&27&29: Area of leaf (m^2)
ρ_leaf <- Para[31,]                        # Eq25&26&27: Density of leaf (kg/m3) - Note: the original paper used different unit for the density of leaves and fruits
RatioM_leaf <- Para[32,]                   # Adjusted: Proportion of leave biomass to a total plant (%)
k_leaf <- Para[33,]                        # Eq25&26: Growth rate of leaves (/day)
C_leaf0 <- Para[36,]                       # Eq25: Initial concentration of contaminants in leaves, set to 0 (mg/kg)

# 6.5 Fruit parameters
RatioQ_fruit <- Para[37,]                  # Adjusted: Ratio of transpiration stream from grain to the total/root (L/day)
area_fruit <- Para[38,]                    # Eq28: Area of grain (m^2)
ρ_fruit <- Para[39,]                       # Eq28: Density of grain (kg/L)
RatioM_fruit <- Para[40,]                  # Adjusted: Proportion of grain biomass to a total plant (%)
k_fruit <- Para[41,]                       # Eq28: Growth rate of grain (/day)
C_fruit0 <- Para[42,]                      # Eq28: Initial concentration of contaminants in grain, set to 0 (mg/kg)

# 6.6 Other general parameters
Density <- Para[71,]                       # Planting density of maize per hectare (kg)
DMYield <- Para[75,]                       # Dry matter yield of maize per year (kg DM/ha)
C_air <- Para[28,]                         # Eq25&26&27&28&29: Concentration of contaminants in air (mg/m^3)
ρ_water <- Para[3,]                        # Eq1: Density of water (kg/L)
t <- Para[34,]                             # Eq25&26&27: time of growth (days)
f <- Para[50,]                             # Eq12: Faraday constant
R <- Para[51,]                             # Eq12: Universal gas constant
temp <- Para[52,]                          # Eq12: absolute temperature (K)
kSetchnov <- Para[22,]                     # Eq16: Setchenov coefficient
A <- Para[18,]                             # Eq15: Factor for ion activity at 20 °C and 1 atm pressure
Vwater_spw <- Para[9,]                     # Eq17: Equals to 1 for soil pore water. To calculate factor of chemical activity when diffusing from soil (Solid) to soil pore water (Solution).
u <- Para[15,]                             # Eq9&20&21: Coefficient required for unit conversion (L/kg)
b <- Para[16,]                             # Eq9&20&21: Correction exponent for differences between plant lipids and octanol
Perm_water_root <- Para[17,]               # Eq10 (only applicable for LogKow >= -2): Membrane permeability of the root cells towards water (m/s)

# 6.7 Cell & Phloem module parameters
pH_cytosol <- Para[44,]                    # Eq2&29: pH in cytosol, set to 7.4.
pH_vacuole <- Para[45,]                    # Eq2&29: pH in vacuole, set to 5.
pH_phloem <- Para[46,]                     # Eq2&29: pH in phloem, set to 5.5.
pH_xylem <- Para[47,]                      # Eq2&29: pH in xylem, set to 8.
U_CytoO <- Para[53,]                       # Eq12: Potential of cytosol to outside (V)
U_VacCyto <- Para[54,]                     # Eq12: Potential of vacuole to cytosol (V)
U_XyCyto <- Para[55,]                      # Eq12: Potential of xylem to cytosol - uncharged to outside (V)
U_PhloCyto <- Para[56,]                    # Eq12: Potential of phloem to cytosol - uncharged to cytosol (V)
Ratio_Phlo <- Para[58,]                    # Ratio of transpiration stream of phloems to that of leaves (%). Note - to be verified again
I_cyto <- Para[20,]                        # Eq15&16: Ionic strength in Davies approximation for calculation of activity coefficient of ions in cell sap (mol/L)
Perm_cellwall <- Para[23,]                 # Eq14: Permeability of cell wall (from original paper, Trapp 2000)
Vwater_xy <- Para[24,]                     # Eq17: Volume fraction of water in xylem (L/L)
Lipid_xy <- Para[59,]                      # Eq9&20&21: Lipid content of xylem (g/g root fresh weight)
Pro_xy <- Para[60,]                        # Eq20&21: Protein content of xylem (g/g root fresh weight)
Vwater_phlo <- Para[25,]                   # Eq17: Volume fraction of water in phloem (L/L)
Lipid_phlo <- Para[61,]                    # Eq9&20&21: Lipid content of phloem (g/g root fresh weight)
Pro_phlo <- Para[62,]                      # Eq20&21: Protein content of phloem (g/g root fresh weight)
V_cyto <- Para[63,]                        # Excel sheet: volume of Cytosol (m^3), taken from Syng model according to the authors
V_xy <- Para[64,]                          # Excel sheet: volume of Xylem (m^3), taken from Syng model according to the authors
V_phlo <- Para[65,]                        # Excel sheet: volume of Phloem (m^3), taken from Syng model according to the authors
V_vac <- Para[66,]                         # Excel sheet: volume of Vacuole (m^3), taken from Syng model according to the authors

# 6.8 Parameters for adjusted soil-crop model： the available dissociation fraction from studies
Fi_spw <- Para[67,]                        # Available mass fraction of ionizable form of active substances in soil pore water
Fi_cyto <- Para[86,]                       # Available mass fraction of ionizable form of active substances in cytosol
Fi_vac <- Para[87,]                        # Available mass fraction of ionizable form of active substances in vacuole
Fi_phlo <- Para[92,]                       # Available mass fraction of ionizable form of active substances in phloem
Fi_xy <- Para[95,]                         # Available mass fraction of ionizable form of active substances in xylem

# 7. Feed-Animal Module parameters
# 7.1 Input parameters
Intake_total <- Para[68,]                  # Daily dry matter intake of animal feed (kg dm/day) 
PerIntake_grain <- Para[89,]               # Proportion of maize grain in daily animal diet (%) 
PerIntake_silage <- Para[78,]              # Proportion of maize silage in daily animal diet (%) 
Lipid_meat <- Para[69,]                    # Lipid content in meat (g/g)
Lipid_milk <- Para[70,]                    # Lipid content in milk (g/g)
ha <- Para[93,]                            # Area of maize farming based on assumption (ha)

# 8. Miscellaneous
# 8.1 Linking variables/changing variables per year
C_soilpost <- Para[1,]                     # Linking variable: Residual concentration of contaminants in soil after maize harvest (mg/kg)
C_fruit <- Para[43,]                       # Linking variable: Concentration of contaminants in fruits/grains (mg/kg)
C_leaf <- Para[49,]                        # Linking variable: Concentration of contaminants in leaves/shoots (mg/kg)

# 8.2 AB setup
ABs <- c("Sulfamethoxazole","Sulfadiazine","Tetracycline","Doxycycline","Flumequine","Lincomycin","Trimethoprim","Tilmicosin")

# 9. Animal-manure-soil and crop-animal simulation
# 9.1 Background setup (manure, soil, and animal intake)
kdeg_manure <-log(2)/DT50manure                                                 # Degradation rate in manure based on experimental half life 
kdegsoil <-log(2)/DT50soil                                                      # Degradation rate in soil based on experimental half life
Npyear <- Npday*365                                                             # Nitrogen production per animal per year

ρWet_soil <- ρDry_soil + Vwater_soil                                            # Calculated: Density of wet soil (kg/L) - assume soil absorb water without volume inflation
# NOT used for now: ρWet_soil <- ρDry_soil*(1-Vwater_soil)+ρ_water*Vwater_soil  # Calculated: Density of wet soil (kg/L) - assume there will be volume inflation
RHOsoil <- ρWet_soil*1000                                                       # Density of soil, with unit transform: (kg/L) ==> (kg/m3)           

DM_maize <- DM_silage/((100-RatioM_root)/100*F_silage/100)                      # Adjusted to incorporate empirical maize data
Yield <- DMYield/DM_maize                                                       # Yield of maize in fresh weight
M_maize <- Yield/Density                                                        # Biomass per maize plant
M_root <- M_maize*RatioM_root                                                   # Root biomass per maize plant
M_leaf <- M_maize*RatioM_leaf                                                   # Leaf biomass per maize plant
M_fruit <- M_maize*RatioM_fruit                                                 # Grain biomass per maize plant

Intake_grain <- Intake_total*PerIntake_grain/100                                # Intake of silage in dry matter
Intake_silage <- Intake_total*PerIntake_silage/100                              # Intake of silage in dry matter
M_silage <- (M_fruit + M_leaf)*F_silage                                         # Total biomass of silage            
RatioSilage_fruit <- M_fruit/(M_silage)                                         # Ratio of grain biomass in total biomass of silage
RatioSilage_leaf <- M_leaf/(M_silage)                                           # Ratio of leaf biomass in total biomass of silage
DM_leaf <- (M_silage*DM_silage - M_fruit*DM_grain)/M_leaf                       # Dry matter content in leaves 

Admin_grain <- Intake_grain/DM_grain*C_fruit                                    # Amount of AMs administered from maize grain
Admin_silage <-                                                                 # Amount of AMs administered from maize silage
  Intake_silage*RatioSilage_fruit/DM_grain*C_fruit + 
  Intake_silage*RatioSilage_leaf/DM_leaf*C_leaf 

Admin_total <- Admin_grain + Admin_silage

# 9.2a Estimating Manure to soil 
Qdose <- DCD*DDDA*Manimal
Qexcr <- (Qdose + Admin_total*365)*Fexcreted                                    # Amount of AMs excreted per animal per year
Cn <- (Qexcr*(exp(-kdeg_manure*(T1storage/2))))/(T1storage*Npyear)              # Concentration of AMs in manure in nitrogen (mg/kg nitrogen)

mperha_soil <- Nst*Cn                                                           # Amount of AMs appplied per hectare
Mperha_soil <- RHOsoil*DEPTHfield*Conv_area_factor                              # Mass of soil per hectare
PECsoil <- mperha_soil/Mperha_soil                                              # Simulated AMs concentration in soil (mg/kg)

C_soil <- PECsoil + C_soilpost                                                  # Loop calculation, C_soilpost = 0 in the first loop.

# 9.2b ***worst-case***: estimating soil concentration using manure concentration of Dutch farm samples
C_manure <- Para[100,]                     # Concentration in manure based on Dutch farm samples (mg/kg)
N_manure <- Para[88,]                      # Nitrogen content in manure ()

Cn <- C_manure/N_manure                                                         # Concentration in manure in nitrogen (mg/kg nitrogen)
mperha_soil <- Nst*Cn                                                           # Amount of AMs appplied per hectare

Mperha_soil <- RHOsoil*DEPTHfield*Conv_area_factor                              # Mass of soil per hectare
PECsoil <- mperha_soil/Mperha_soil                                              # Simulated AMs concentration in soil (mg/kg)
C_soil <- PECsoil + C_soilpost                                                  # Loop calculation, C_soilpost = 0 in the first loop.

#Qexcr <- Cn*T1storage*Npyear/(exp(-kdeg_manure*(T1storage/2)))                  # Retrospectively estimate the amount of AM excretion
#DDDA <- (Qexcr/(Fexcreted*365) - Admin_total)/Manimal                           # Retrospectively estimate the DDDA

# 9.3 Biotransfer factor
lgBTF <- -0.099*lgKow_n^2 + 1.07*lgKow_n - 3.56                                 # Parameters obtained based on empirical studies as Carmen stated.
BTF <- 10^lgBTF

# 9.4a Concentration in meat and milk
C_meat <- BTF*
  (Qdose/365 + Admin_total)*Lipid_meat/100                                      # the original paper did not multiple with t_animal.
C_milk <- BTF*(Qdose/365 + Admin_total)*Lipid_milk/100                          # the original paper did not multiple with t_milking.

# 9.4b ***worst-case***: Concentration in meat and milk and inspection
C_meat <- C_fruit                                                               # create a variable that contains null for now.
C_milk <- C_fruit                                                               # create a variable that contains null for now.
Meatresult <- C_fruit                                                           # create a variable that contains null for now.
Milkresult <- C_fruit                                                           # create a variable that contains null for now.

# 9.5a verification of animal-related mass balance (skipped for the ***worst-case*** scenario)
Q_meat <- C_meat*Manimal*Ratio_meat
Qmeat_admin <- (Qdose/365 + Admin_total)*t_animal    
Meat <- Qmeat_admin - Q_meat - Qexcr/365*t_animal                               # Qexcr adjusted due to various animal lifecycles
Meatresult <- Meat

for (i in 1:length(Meat)) {
  if (Meat[i] < 0){
    Meatresult[i] <- c("check failed!!")
  } else {
    Meatresult[i] <- c("check passed!")
  } 
}
Meatresult <- as.matrix(Meatresult)
colnames(Meatresult) <- c("Meat mass balance check ")

Qmilk_admin <- (Qdose/365 + Admin_total)*t_milking
Q_milk <- C_milk*Y_milk*t_milking
Milk <- Qmilk_admin - Q_milk - Qexcr*t_milking/365
Milkresult <- Milk

for (i in 1:length(Milk)) {
  if (Milk[i] < 0){
    Milkresult[i] <- c("check failed!!")
  } else {
    Milkresult[i] <- c("check passed!")
  } 
}
Milkresult <- as.matrix(Milkresult)
colnames(Milkresult) <- c("Milk mass balance check ")

# 10. Soil-crop simulation
# 10.1 Dissociation fraction with chemical activity correction in soil pore water
Nr_ABs <- 8
DisF <- -z                                                                      # NOTE! Estimation in Paper (DisF = -z)and Excel sheet (DisF = z) was contradictory
Dis_spw <- 10^(DisF*(pH_soil-pKa))                                              # Eq2: Key element of the pH-dependent dissociation model

KAct_n_spw <- 10^(kSetchnov*I_soil)                                             # Eq16: Activity coefficient for neutrals in soil pore water
KAct_i_spw <-                                                                   # Eq15: Activity coefficient for ions in soil pore water
  10^(-A*z^2*((I_soil)^(1/2)/(1 + ((I_soil)^(1/2))) - 0.3*I_soil))

FAct_n_spw <-                                                                   # Eq17: Dissociation fraction for neutrals in soil pore water
  1/(Vwater_spw/KAct_n_spw + Vwater_spw*Dis_spw/KAct_i_spw) 
FAct_i_spw <- FAct_n_spw*Dis_spw                                                # Eq19: Dissociation fraction for ions in soil pore water

#FAct_i_spw <- Fi_spw                                                           # Adjusted soil-crop model
#FAct_n_spw <- 1 - FAct_i_spw                                                   # Adjusted soil-crop model

# 10.2a Koc_n estimation (Neutral fraction): Organic carbon to water partitioning coefficient
lgKoc_n <- lgKow_n                                                              # ONLY used to create the variable. Values are calculated as follows
for (i in 1:Nr_ABs) {
  if (z[i] == -1) {                                                             # z == -1 indicates the molecule is acid neutral, Franco & Trapp 2008 for weak organic electrolytes(SI)
    lgKoc_n[i] <- 0.54*lgKow_n[i] + 1.11
  } else if (z[i] == 1) {                                                       # z == +1 indicates the molecule is base neutral, Franco & Trapp 2008 for weak organic electrolytes (SI)
    lgKoc_n[i] <- 0.42*lgKow_n[i] + 1.34
  } else if (z[i] == 0) {                                                       # z == 0 indicates the molecule is neutral based on ECHA neutral (SI)
    lgKoc_n[i] <- 0.81*lgKow_n[i] + 0.1
  }
}
Koc_n <- 10^lgKoc_n

# 10.2b Koc_i estimation (ionizable fraction - monovalent)
lgKoc_i <- lgKow_n                                                              # ONLY used to create the variable. Values are calculated as follows 
for (i in 1:Nr_ABs) {
  if (z[i] == -1) {                                                             # z == -1 indicates the molecule is anion, Franco & Trapp 2008 for weak organic electrolytes(SI)
    lgKoc_i[i] <- 0.11*lgKow_n[i] + 1.54
  } else {                                                                      # z == +1 indicates the molecule is cation, Franco & Trapp 2008 for weak organic electrolytes (SI)
    lgKoc_i[i] <- 0.47*lgKow_n[i] + 1.95
  } 
  
}
Koc_i <- 10^lgKoc_i
Koc <- Koc_n*FAct_n_spw + Koc_i*FAct_i_spw                                      # Koc estimation (sum of neutral and ion forms of AMs)
# NOT used for now. Koc <- 10^(0.81*lgKow_n+0.1)                                # Koc estimation (considering the AMs exists only in neutral form)

# 10.3 Calculating Kws (OC may change per loop but NOT considered in our model) 
Kws <- (ρWet_soil)/(OC*Koc*ρDry_soil + Vwater_soil)                             # Eq1: Partition Coefficient for water to soil

# P10.4 Permeability of AMs through corn root cells (neutral and ion forms) - PPCP Model
Kow_n <- 10^lgKow_n
lgKow_i <- lgKow_n-3.5                                                          # Text below Eq14 - Estimate lgKow of AMs in ion form based on assumption from Trapp & Horobin (2005)
Perm_i_cyto <- 10^(lgKow_i-6.7)                                                 # Eq14: Estimation of permeability of ions to root cell
Perm_i_cyto <- 1/(1/Perm_i_cyto + 1/Perm_cellwall)                              # Equation obtained from excel sheet of Trapp but no other explanations in the paper.
Perm_n_cyto <- 10^(lgKow_n-6.7)                                                 # Eq14: Estimation of permeability of neutral to root cell
Perm_n_cyto <- 1/(1/Perm_n_cyto + 1/Perm_cellwall)                              # Equation obtained from excel sheet of Trapp but no other explanations in the paper.

Perm_cyto <- Perm_n_cyto*FAct_n_spw + Perm_i_cyto*FAct_i_spw                    # Sum of neutral and ion permeability

# P10.5 Correction factor for root cell permeability - PPCP Model
F_root <- Perm_cyto/Perm_water_root                                             # Eq10: Calculation of correction factor (F_root must <=1 as advective uptake of solution is never faster than that of water)
for (i in 1:Nr_ABs){
  if (F_root[i] > 1){                                                           # Eq10: Differentiation of F_root for highly polar compounds and other compounds
    F_root[i] <- 1
  } else {                                                                    
    F_root[i] <- F_root[i]
  }
}

# P10.6 Cell model: Apparent Parition Coefficient from Outside (soil pore water) to Cytoplasm/Cytosol of root cell ==> (CytoO)
# P10.6.1 Flux of AMs from Outside (soil pore water) to Cytoplasm/Cytosol of root cell => (CytoO)
N_CytoO <- z*U_CytoO*f/(R*temp)                                                 # Text below Eq12: calculate Nernst number for the permeation of Cytoplasm to outside 
Flx_in_CytoO <-                                                                 # Eq12: calculate Inflow of AMs to Cytosol
  FAct_n_spw*Perm_n_cyto + (FAct_i_spw*Perm_i_cyto*N_CytoO)/(exp(N_CytoO)-1)       

# P10.6.2 Flux of AMs from Cytoplasm/Cytosol to Outside (soil pore water) ==> (OCyto)
Khsa_cor <- Khsa/60                                                             # Text below Eq21: Unit change of Khsa (L/mol => L/kg, using a molar mass of 60 000 g/mol for human serum albumin)
kSorp_n_root <- Lipid_root*u*(Kow_n^b) + Pro_root*Khsa_cor                      # Eq20: calculate sorption coefficient (for neutral) considering the effects of lipid and protein
kSorp_i_root <- Lipid_root*u*((10^lgKow_i)^b) + Pro_root*Khsa_cor               # Eq20: calculate sorption coefficient (for ion) considering the effects of lipid and protein

kAct_n_cyto <- 10^(kSetchnov*I_cyto)                                            # Eq16: calculate activity coefficient for neutrals in root cytosol
kAct_i_cyto <-                                                                  # Eq15: calculate activity coefficient for ions in root cytosol
  10^(-A*z^2*(I_cyto^(1/2)/(1 + (I_cyto^(1/2))) - 0.3*I_cyto))

pKa_cor <- pKa - z                                                              # Text below Eq3: Corrected pKa when ionization is despressed due to the presence of membrane
Dis_cyto <- 10^(DisF*(pH_cytosol-pKa_cor))                                      # Eq2: Key element of the pH-dependent dissociation model
FAct_n_cyto <-                                                                  # Eq17: calculate dissociation factor for neutrals in root cytosol
  1/((Vwater_root/kAct_n_cyto) + (kSorp_n_root/kAct_n_cyto)
     + (Vwater_root*Dis_cyto/kAct_i_cyto) + (kSorp_i_root*Dis_cyto/kAct_i_cyto))# CAUTION! Difference between Paper and Excel sheet of Trapp due to the additional "Dis_cyto" at the last item.
FAct_i_cyto <- FAct_n_cyto*Dis_cyto                                             # Eq19: calculate dissociation factor for ions in root cytosol

#FAct_i_cyto <- Fi_cyto                                                         # Adjusted soil-crop model
#FAct_n_cyto <- 1 - FAct_i_cyto                                                 # Adjusted soil-crop model

Flx_out_CytoO <-                                                                # Eq12: calculate Outflow of AMs from Cytosol
  (FAct_n_cyto*Perm_n_cyto + 
     FAct_i_cyto*Perm_i_cyto*exp(N_CytoO)*N_CytoO/(exp(N_CytoO)-1))

# P10.6.3 Apparent partition coefficient of AMs: Cytoplasm/Cytosol to Outside (soil pore water) (CytoO)
KAP_CytoO <- Flx_in_CytoO/Flx_out_CytoO                                         # Eq22: from Outside to Cytosol, equals to C_cyto/Cw_soil


# P10.7 Cell model: Apparent Partition Coefficient from Cytoplasm/Cytosol to Xylem ==> (XyCyto)
# P10.7.1 Flux of AMs from Cytoplasm/Cytosol to Xylem => (XyCyto)
N_XyCyto <- z*U_XyCyto*f/(R*temp)                                               # Text below Eq12: calculate Nernst number for the permeation of Xylem to Cytoplasm
Flx_in_XyCyto <-                                                                # Eq12: calculate Inflow of AMs to Xylem
  FAct_n_cyto*Perm_n_cyto + (FAct_i_cyto*Perm_i_cyto*N_XyCyto)/(exp(N_XyCyto)-1)     

# P10.7.2 Flux of AMs from Xylem to Cytoplasm/Cytosol => (CytoXy)
kSorp_n_xy <- Lipid_xy*u*(Kow_n^b) + Pro_xy*Khsa_cor                            # Eq20: calculate sorption coefficient (for neutral) considering the effects of lipid and protein
kSorp_i_xy <- Lipid_xy*u*((10^lgKow_i)^b) + Pro_xy*Khsa_cor                     # Eq20: calculate sorption coefficient (for ion) considering the effects of lipid and protein

I_xy <- I_soil                                                                  # Ion strength in xylem is the same as that of soil pore water since xylem contains only water, assumed by the authors. 
kAct_n_xy <- 10^(kSetchnov*I_xy)                                                # Eq16: calculate activity coefficient for neutrals in xylem
kAct_i_xy <-                                                                    # Eq15: calculate activity coefficient for ions in xylem
  10^(-A*z^2*(I_xy^(1/2)/(1 + (I_xy^(1/2))) - 0.3*I_xy))

Dis_xy <- 10^(DisF*(pH_xylem-pKa_cor))                                          # Eq2: Key element of the pH-dependent dissociation model
FAct_n_xy <-                                                                    # Eq17: calculate dissociation factor for neutrals in xylem
  1/((Vwater_xy/kAct_n_xy) + (kSorp_n_xy/kAct_n_xy)
     + (Vwater_xy*Dis_xy/kAct_i_xy) + (kSorp_i_xy*Dis_xy/kAct_i_xy))            # CAUTION! Difference between Paper and Excel sheet due to the additional "Dis_xy" at the last item.
FAct_i_xy <- FAct_n_xy*Dis_xy                                                   # Eq19: calculate dissociation factor for ions in root xylem

#FAct_i_xy <- Fi_xy                                                              # Adjusted soil-crop model
#FAct_n_xy <- 1 - FAct_i_xy                                                      # Adjusted soil-crop model

Flx_out_XyCyto <-                                                               # Eq12: calculate Outflow of AMs from Xylem
  (FAct_n_xy*Perm_n_cyto + 
     FAct_i_xy*Perm_i_cyto*exp(N_XyCyto)*N_XyCyto/(exp(N_XyCyto)-1))

# P10.7.3 Apparent partition coefficient of AMs: Xylem to Cytoplasm/Cytosol (XyCyto)
KAP_XyCyto <- Flx_in_XyCyto/Flx_out_XyCyto                                      # Eq22: from Cytosol to Xylem, equals to C_xy/C_cyto

# P10.8 Cell model: Apparent Partition Coefficient from Cytoplasm/Cytosol to Phloem ==> (PhloCyto)
# P10.8.1 Flux of AMs from Cytoplasm/Cytosol to Phloem => (PhloCyto)
N_PhloCyto <- z*U_PhloCyto*f/(R*temp)                                           # Text below Eq12: calculate Nernst number for the permeation of Phloem to Cytoplasm
Flx_in_PhloCyto <-                                                              # Eq12: calculate Inflow of AMs to Phloem
  FAct_n_cyto*Perm_n_cyto + (FAct_i_cyto*Perm_i_cyto*N_PhloCyto)/(exp(N_PhloCyto)-1)     


# P10.8.2 Flux of AMs from Phloem to Cytoplasm/Cytosol => (CytoPhlo)
kSorp_n_phlo <- Lipid_phlo*u*(Kow_n^b) + Pro_phlo*Khsa_cor                      # Eq20: calculate sorption coefficient (for neutral) considering the effects of lipid and protein
kSorp_i_phlo <- Lipid_phlo*u*((10^lgKow_i)^b) + Pro_phlo*Khsa_cor               # Eq20: calculate sorption coefficient (for ion) considering the effects of lipid and protein

I_phlo <- I_cyto                                                                # Ion strength in phloem is the same as that of cytosol as plant cell, assumed by the authors. 
kAct_n_phlo <- 10^(kSetchnov*I_phlo)                                            # Eq16: calculate activity coefficient for neutrals in phloem
kAct_i_phlo <-                                                                  # Eq15: calculate activity coefficient for ions in phloem
  10^(-A*z^2*(I_phlo^(1/2)/(1 + (I_phlo^(1/2))) - 0.3*I_phlo))

Dis_phlo <- 10^(DisF*(pH_phloem-pKa_cor))                                       # Eq2: Key element of the pH-dependent dissociation model
FAct_n_phlo <-                                                                  # Eq17: calculate dissociation factor for neutrals in phloem
  1/((Vwater_phlo/kAct_n_phlo) + (kSorp_n_phlo/kAct_n_phlo)
     + (Vwater_phlo*Dis_phlo/kAct_i_phlo) + (kSorp_i_phlo*Dis_phlo/kAct_i_phlo))# CAUTION! Difference between Paper and Excel sheet due to the additional "Dis_phlo" at the last item.
FAct_i_phlo <- FAct_n_phlo*Dis_phlo                                             # Eq19: calculate dissociation factor for ions in phloem

#FAct_i_phlo <- Fi_phlo                                                          # Adjusted soil-crop model
#FAct_n_phlo <- 1 - FAct_i_phlo                                                  # Adjusted soil-crop model

Flx_out_PhloCyto <-                                                             # Eq12: calculate Outflow of AMs from Phloem
  (FAct_n_phlo*Perm_n_cyto + 
     FAct_i_phlo*Perm_i_cyto*exp(N_PhloCyto)*N_PhloCyto/(exp(N_PhloCyto)-1))

# P10.8.3 Apparent partition coefficient of AMs: Phloem to Cytoplasm/Cytosol (PhloCyto)
KAP_PhloCyto <- Flx_in_PhloCyto/Flx_out_PhloCyto                                # Eq22: from Cytosol to Phloem, equals to C_phlo/C_cyto

# P10.9 Cell model: Apparent Parition Coefficient from Cytoplasm/Cytosol to Vacuole ==> (VacCyto)
# P10.9.1 Flux of AMs from Cytoplasm/Cytosol to Vacuole ==> (VacCyto)
N_VacCyto <- z*U_VacCyto*f/(R*temp)                                             # Text below Eq12: calculate Nernst number for the permeation of Vacuole to Cytoplasm
Perm_n_vac <- 10^(lgKow_n-6.7)                                                  # CAUTION! strage calculation based on the equation in Excel sheet from Trapp.

Flx_in_VacCyto <-                                                               # CAUTION! Difference in Excel sheet due to the calculation of "Perm_n_vac"
  FAct_n_cyto*Perm_n_vac + (FAct_i_cyto*Perm_i_cyto*N_VacCyto)/(exp(N_VacCyto)-1)     
# NOT used for now: Flx_in_VacCyto <-                                            # Eq12: calculate Inflow of AMs to Vacuole
#  FAct_n_cyto*Perm_n_cyto + (FAct_i_cyto*Perm_i_cyto*N_VacCyto)/(exp(N_VacCyto)-1)     

# P10.9.2 Flux of AMs from Vacuole to Cytoplasm/Cytosol ==> (CytoVac)
Vwater_vac <- Vwater_root                                                       # Volume fraction of water in vacuole equals to that of root cell (L/L)
Lipid_vac <- Lipid_root                                                         # Content of lipid in vacuole equals to that of root cell (L/L)
Pro_vac <- Pro_root                                                             # Content of protein in vacuole equals to that of root cell (L/L)

kSorp_n_vac <- Lipid_vac*u*(Kow_n^b) + Pro_vac*Khsa_cor                         # Eq20: calculate sorption coefficient (for neutral) considering the effects of lipid and protein
kSorp_i_vac <- Lipid_vac*u*((10^lgKow_i)^b) + Pro_vac*Khsa_cor                  # Eq20: calculate sorption coefficient (for ion) considering the effects of lipid and protein

I_vac <- I_cyto                                                                 # Ion strength in vacuole is the same as that of cytosol, assumed by the authors. 
kAct_n_vac <- 10^(kSetchnov*I_vac)                                              # Eq16: calculate activity coefficient for neutrals in vacuole
kAct_i_vac <-                                                                   # Eq15: calculate activity coefficient for ions in vacuole
  10^(-A*z^2*(I_vac^(1/2)/(1 + (I_vac^(1/2))) - 0.3*I_vac))

Dis_vac <- 10^(DisF*(pH_vacuole-pKa_cor))                                       # Eq2: Key element of the pH-dependent dissociation model
FAct_n_vac <-                                                                   # Eq17: calculate dissociation factor for neutrals in vacuole
  1/((Vwater_vac/kAct_n_vac) + (kSorp_n_vac/kAct_n_vac)
     + (Vwater_vac*Dis_vac/kAct_i_vac) + (kSorp_i_vac*Dis_vac/kAct_i_vac))      # CAUTION! Difference between Paper and Excel sheet due to the additional "Dis_vac" at the last item.
FAct_i_vac <- FAct_n_vac*Dis_vac                                                # Eq19: Estimation of dissociation factor for ions in vacuole

#FAct_i_vac <- Fi_vac                                                            # Adjusted soil-crop model
#FAct_n_vac <- 1 - FAct_i_vac                                                    # Adjusted soil-crop model

Flx_out_VacCyto <-                                                              # Eq12: calculate Outflow of AMs from Vacuole (with strange perm_vac value)
  (FAct_n_vac*Perm_n_vac + 
     FAct_i_vac*Perm_i_cyto*exp(N_VacCyto)*N_VacCyto/(exp(N_VacCyto)-1))
# NOT used for now Flx_out_VacCyto <-                                            # Eq12: calculate Outflow of AMs from Vacuole
#  (FAct_n_vac*Perm_n_cyto + 
#     FAct_i_vac*Perm_i_cyto*exp(N_VacCyto)*N_VacCyto/(exp(N_VacCyto)-1))

# P10.9.3 Apparent partition coefficient of AMs: Vacuole to Cytoplasm/Cytosol (VacCyto)
KAP_VacCyto <- Flx_in_VacCyto/Flx_out_VacCyto                                   # Eq22: from Cytosol to Vacuole, equals to C_vac/C_cyto

# P10.10 Partition coefficients across compartments
# P10.10.1 Partition coefficients of root to soil pore water Krw (only vacuole + cytosol)
KAP_VacO <- KAP_VacCyto*KAP_CytoO                                               # Calculate apparent partition coefficient of Vacuole to the outside (soil pore water)
Krw <- (KAP_CytoO*V_cyto + KAP_VacO*V_vac)/(V_cyto + V_vac)                     # Calculate partition coefficient of root to water

# P10.10.2 Partition coefficient of xylem to soil pore water KXyW
KXyW <- KAP_CytoO*KAP_XyCyto                                                    # Estimate partitioning coefficient of xylem to soil pore water
for (i in 1:Nr_ABs){                                                            # Eq23: Avoidance of violation of the Law of Mass Conservation ==> KXyW <= 1
  if (KXyW[i] > 1){
    KXyW[i] <- 1
  } else {
    KXyW[i] <- KXyW[i]
  }
}

F_root <- Perm_cyto/Perm_water_root                                             # Eq10: calculate correction factor (F_root must <=1 as advective uptake of solution is never faster than that of water)
for (i in 1:Nr_ABs){
  if (F_root[i] > 1){                                                           # Eq10: Differentiation of F_root for highly polar compounds and other compounds
    F_root[i] <- 1
  } else {                                                                    
    F_root[i] <- F_root[i]
  }
}

# P10.10.3 Partition coefficient of xylem to root KXyR
KXyR <- KXyW/Krw                                                                # Partition coefficient of Xylem to Root

# P10.10.4 Partition coefficient of phloem to leaf KPhloL                        
KPhloO <- KAP_PhloCyto*KAP_CytoO                                                # Partition coefficient of Phloem to Outside (soil pore water)

Klw <- Krw                                                                      # Partition coefficient of Leaf to Water (soil pore water), considering only cytosol and vacuole, thus same as Krw when steady-state.
KPhloL <- KPhloO/Klw                                                            # Eq32: Partition coefficient of Phloem to Leaf 

# P10.10.5 Partition coefficient of leaf to air Kla (negligible diffusion of ion molecule to the air), considering only cytosol and vacuole
Klw_onlyn <-                                                                    # CAUTION! Unknown source of equations (maybe Eq30?), for calculation of Kla.
  ((kSorp_n_root + Vwater_root)*V_cyto + (kSorp_n_vac + Vwater_vac)*V_vac)/
  (V_cyto + V_vac)
FAct_onlyn_cyto <- 1/(1 + 10^(DisF*(pH_cytosol - pKa)))                         # No Setchnov, No membrane
Kla <- (Klw_onlyn/Kaw)/FAct_onlyn_cyto                                          # Partition coefficient of Leaf to Air (only neutral will diffuse)

# P10.11 Phloem transport
Q_leaf <- RatioQ_leaf*Q_root                                                    # Q_leaf + Q_fruit = Q_root
Q_fruit <- Q_root - Q_leaf 
Qphlo_leaf <- Q_leaf*Ratio_Phlo                                                 # Text below Eq32: transpiration stream of Phloem to leaf
Qphlo_fruit <- Q_fruit*Ratio_Phlo                                               # Text below Eq32: transpiration stream of Phloem to fruit

# P10.12 Loss of contaminants from leaves
loss2air_leaf <- (g*area_leaf*ρ_leaf/Kla)/M_leaf                                # Eq29: AMs lost to Air from Leaf (neutral form only)
loss2root_leaf <- KPhloL*Qphlo_leaf/M_leaf                                      # Eq32: loss from Leaf to Roots via Phloem
loss2fruit_leaf <- KPhloL*Qphlo_fruit/M_leaf                                    # CAUTION!: different from the equation in Excel sheet from Trapp... Eq32: loss from Leaf to Fruit via Phloem

# P10.13 Variables representing different movement of ABs 
soil2root <- F_root*Q_root*KXyW*Kws/M_root                                      # Represent the uptake rate of AMs from Soil to Root via Xylem
loss_root <- k_root + kdg + Q_root*KXyR/M_root                                  # Represent the loss rate of AMs from Root due to growth dilution & metabolism and loss via Xylem to Leaf and/or Fruit
root2leaf <- Q_leaf*KXyR/M_leaf                                                 # Represent the translocation rate of AMs from Root to Leaf via Xylem
leaf2root <- KPhloL*Qphlo_leaf/M_root                                           # Eq34: AMs gain in Roots from Leaves via Phloem
loss_leaf <- loss2air_leaf + loss2root_leaf + loss2fruit_leaf + k_leaf + kdg    # Adding up Leaf loss to air, Leaf loss to Root via Xylem, Leaf loss to Fruit via Xylem, and growth dilution & metabolism
airuptake_leaf <- g*area_leaf*C_air/M_leaf                                      # Eq24 (middle item): AMs deposition from Air

# P10.14 Root, leaf, fruit concentration (fresh weight)
Ratio_xy <- Ratio_Phlo                                                          # Fraction Xylem to Fruits acccording to Excel sheet
Q_xy <- Q_root*Ratio_xy                                                         # Transpiration stream Xylem

C_root <-                                                                       # According to excel sheet of Trapp
  (soil2root*loss_leaf*C_soil + airuptake_leaf*leaf2root)/(loss_root*loss_leaf - leaf2root*root2leaf)

C_leaf <- (root2leaf*C_root + airuptake_leaf)/loss_leaf                         # According to excel sheet of Trapp

root2fruit <- C_root*KXyR*Q_xy/M_fruit                                          # Uptake from roots via xylem
leaf2fruit <- KPhloL*Qphlo_fruit/M_fruit                                        # Uptake via phloem from leaves
air2fruit <- area_fruit*g*C_air/M_fruit                                         # Uptake from air
loss2air_fruit <- 1000*ρ_fruit*area_fruit*g/(Kla*M_fruit)                       # CAUTION! Loss to air calculation differs with the unit of ρ_fruit between Paper (kg/L) and Excel sheet (kg/m3), hence *1000
loss_fruit <- loss2air_fruit + k_fruit + kdg                                    # Total loss of fruit

C_fruit <- (root2fruit + leaf2fruit + air2fruit)/loss_fruit                     # According to excel sheet of Trapp

# 11. Mass verification in soil-crop module
# 11.1 Mass of AMs in crop tissues
mperha_root <- C_root*M_root*Density                                            # Mass in roots per hectare
mperha_leaf <- C_leaf*M_leaf*Density                                            # Mass in shoots per hectare
mperha_fruit <- C_fruit*M_fruit*Density                                         # Mass in grains per hectare
Soilresult <- mperha_soil                                                       # Create the variable for soil-crop mass balance check

# 11.2a Mass balance check and result summary
mperha_soilpost <- mperha_soil - mperha_root - mperha_leaf - mperha_fruit       # Mass in soil per hectare after harvest
for (i in 1:length(mperha_soilpost)){                                           
  if (mperha_soilpost[i] < 0){
    Soilresult[i] <- c("check failed!!")
  } else {
    Soilresult[i] <- c("check passed!!")
  }
}

Soilresult <- as.matrix(Soilresult)
colnames(Soilresult) <- c("Soil mass balance check")
MassCheck <- cbind(Meatresult, Milkresult, Soilresult)
print(MassCheck)

# 11.2b ***worst-case****: Mass balance check and result summary
mperha_soilpost <- mperha_soil - mperha_root - mperha_leaf - mperha_fruit       # Mass in soil per hectare after harvest
for (i in 1:length(mperha_soilpost)){                                           
  if (mperha_soilpost[i] < 0){
    Soilresult[i] <- c("check failed!!")
  } else {
    Soilresult[i] <- c("check passed!!")
  }
}

Soilresult <- as.matrix(Soilresult)
colnames(Soilresult) <- c("Soil mass balance check")
print(Soilresult)

# 11.3 Soilpost concentration
for (i in 1:Nr_ABs){                                                            # mass correction
  if (mperha_soilpost[i] > 0){
    mperha_soilpost[i] <- mperha_soilpost[i]
  } else {
    mperha_soilpost[i] <- 0
  }
}

C_soilpost <- mperha_soilpost*exp(-kdegsoil*(365-t))/Mperha_soil                # considering degradation in soil
C_soilpost
# 12. Circularity calculation
# 12.1 Loop 1 (the first year)
Loop_1 <- cbind(DCD*DDDA, Admin_total, C_milk, C_meat, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost)
print(Loop_1)

Loop_1W <- cbind(Admin_total, C_manure, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost, C_milk, C_meat)
print(Loop_1W)

# 12.2a Function creation for concentration simulation and mass balance check
Loop_calculation <- function (C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root){
  # Calculate animal intake of AMs in the new loop 
  Admin_grain <- Intake_grain/DM_grain*C_fruit                                  
  Admin_silage <-                                                               
    Intake_silage*RatioSilage_fruit/DM_grain*C_fruit + 
    Intake_silage*RatioSilage_leaf/DM_leaf*C_leaf
  Admin_total <- Admin_grain + Admin_silage
  
  # Calculate the concentration in meat and milk in the new loop 
  C_meat <- BTF*(Qdose/365 + Admin_total)*Lipid_meat/100
  C_milk <- BTF*(Qdose/365 + Admin_total)*Lipid_milk/100

  # Calculate the concentration in soil in the new loop
  Qexcr <- (Qdose + Admin_total*365)*Fexcreted                
  Cn <- (Qexcr*(exp(-kdeg_manure*(T1storage/2))))/(T1storage*Npyear)
  mperha_soil <- Nst*Cn                                                       
  Mperha_soil <- RHOsoil*DEPTHfield*Conv_area_factor                         
  PECsoil <- mperha_soil/Mperha_soil                                           
  C_soil <- PECsoil + C_soilpost
  
  # Mass balance verification in the animal-related modules
  Q_meat <- C_meat*Manimal*Ratio_meat
  Qmeat_admin <- (Qdose/365 + Admin_total)*t_animal             
  Meat <- Qmeat_admin - Q_meat - Qexcr*t_animal/T1storage
  for (i in 1:length(Meat)) {
    if (Meat[i] < 0){
      Meatresult[i] <- c("check failed!!")
    } else {
      Meatresult[i] <- c("check passed!")
    } 
  }
  Meatresult <- as.matrix(Meatresult)
  colnames(Meatresult) <- c("Meat mass balance check ")
  
  Qmilk_admin <- (Qdose/365 + Admin_silage + Admin_grain)*t_milking
  Q_milk <- C_milk*Y_milk*t_milking
  Milk <- Qmilk_admin - Q_milk - Qexcr*t_milking/t_animal
  for (i in 1:length(Milk)) {
    if (Milk[i] < 0){
      Milkresult[i] <- c("check failed!!")
    } else {
      Milkresult[i] <- c("check passed!")
    } 
  }
  Milkresult <- as.matrix(Milkresult)
  colnames(Milkresult) <- c("Milk mass balance check ")
  
  # Calculate the concentration in plant tissues in the new loop
  C_root <- (soil2root*loss_leaf*C_soil + airuptake_leaf*leaf2root)/(loss_root*loss_leaf - leaf2root*root2leaf)
  C_leaf <- (root2leaf*C_root + airuptake_leaf)/loss_leaf
  root2fruit <- C_root*KXyR*Q_xy/M_fruit
  C_fruit <- (root2fruit + leaf2fruit + air2fruit)/loss_fruit
  
  # Mass balance verification in the crop-related modules
  mperha_root <- C_root*M_root*Density                                          
  mperha_leaf <- C_leaf*M_leaf*Density                                           
  mperha_fruit <- C_fruit*M_fruit*Density                                       
  mperha_soilpost <- mperha_soil - mperha_root - mperha_leaf - mperha_fruit     
  for (i in 1:length(mperha_soilpost)){                                           
    if (mperha_soilpost[i] < 0){
      Soilresult[i] <- c("check failed!!")
    } else {
      Soilresult[i] <- c("check passed!!")
    }
  }
  Soilresult <- as.matrix(Soilresult)
  colnames(Soilresult) <- c("Soil mass balance check")
  
  # Check result summary
  MassCheck <- cbind(Meatresult, Milkresult, Soilresult)
  
  # Soilpost concentration with correction
  for (i in 1:Nr_ABs){             
    if (mperha_soilpost[i] > 0){
      mperha_soilpost[i] <- mperha_soilpost[i]
    } else {
      mperha_soilpost[i] <- 0
    }
  }
  C_soilpost <- mperha_soilpost*exp(-kdegsoil*(365-t))/Mperha_soil
  
  # Return results
  return(list(
    Admin_total = Admin_total,
    C_meat = C_meat, C_milk = C_milk, 
    C_soil = C_soil, Cn = Cn,
    C_root = C_root, C_leaf = C_leaf, C_fruit = C_fruit, 
    MassCheck = MassCheck,
    C_soilpost = C_soilpost))
}

# 12.2b ***worst-case***: Function creation for concentration simulation and mass balance check
Loop_calculation <- function (C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root){
  # Calculate animal intake of AMs in the new loop 
  Admin_grain <- Intake_grain/DM_grain*C_fruit                                  
  Admin_silage <-                                                               
    Intake_silage*RatioSilage_fruit/DM_grain*C_fruit + 
    Intake_silage*RatioSilage_leaf/DM_leaf*C_leaf
  Admin_total <- Admin_grain + Admin_silage
  
  # Calculate the concentration in meat and milk in the new loop 
  C_meat <- BTF*(Admin_total)*Lipid_meat/100
  C_milk <- BTF*(Admin_total)*Lipid_milk/100
  
  # Calculate the concentration in soil in the new loop
  Qexcr <- (Admin_total)*365*Fexcreted                
  Cn <- (Qexcr*(exp(-kdeg_manure*(T1storage/2))))/(T1storage*Npyear)
  mperha_soil <- Nst*Cn                                                       
  Mperha_soil <- RHOsoil*DEPTHfield*Conv_area_factor                         
  PECsoil <- mperha_soil/Mperha_soil                                           
  C_soil <- PECsoil + C_soilpost
  
  # Mass balance verification in the animal-related modules
  Q_meat <- C_meat*Manimal*Ratio_meat
  Qmeat_admin <- (Admin_total)*t_animal             
  Meat <- Qmeat_admin - Q_meat - Qexcr*t_animal/T1storage
  for (i in 1:length(Meat)) {
    if (Meat[i] < 0){
      Meatresult[i] <- c("check failed!!")
    } else {
      Meatresult[i] <- c("check passed!")
    } 
  }
  Meatresult <- as.matrix(Meatresult)
  colnames(Meatresult) <- c("Meat mass balance check ")
  
  Qmilk_admin <- (Admin_silage + Admin_grain)*t_milking
  Q_milk <- C_milk*Y_milk*t_milking
  Milk <- Qmilk_admin - Q_milk - Qexcr*t_milking/t_animal
  for (i in 1:length(Milk)) {
    if (Milk[i] < 0){
      Milkresult[i] <- c("check failed!!")
    } else {
      Milkresult[i] <- c("check passed!")
    } 
  }
  Milkresult <- as.matrix(Milkresult)
  colnames(Milkresult) <- c("Milk mass balance check ")
  
  # Calculate the concentration in plant tissues in the new loop
  C_root <- (soil2root*loss_leaf*C_soil + airuptake_leaf*leaf2root)/(loss_root*loss_leaf - leaf2root*root2leaf)
  C_leaf <- (root2leaf*C_root + airuptake_leaf)/loss_leaf
  root2fruit <- C_root*KXyR*Q_xy/M_fruit
  C_fruit <- (root2fruit + leaf2fruit + air2fruit)/loss_fruit
  
  # Mass balance verification in the crop-related modules
  mperha_root <- C_root*M_root*Density                                          
  mperha_leaf <- C_leaf*M_leaf*Density                                           
  mperha_fruit <- C_fruit*M_fruit*Density                                       
  mperha_soilpost <- mperha_soil - mperha_root - mperha_leaf - mperha_fruit     
  for (i in 1:length(mperha_soilpost)){                                           
    if (mperha_soilpost[i] < 0){
      Soilresult[i] <- c("check failed!!")
    } else {
      Soilresult[i] <- c("check passed!!")
    }
  }
  Soilresult <- as.matrix(Soilresult)
  colnames(Soilresult) <- c("Soil mass balance check")
  
  # Check result summary
  MassCheck <- cbind(Meatresult, Milkresult, Soilresult)
  
  # Soilpost concentration with correction
  for (i in 1:Nr_ABs){             
    if (mperha_soilpost[i] > 0){
      mperha_soilpost[i] <- mperha_soilpost[i]
    } else {
      mperha_soilpost[i] <- 0
    }
  }
  C_soilpost <- mperha_soilpost*exp(-kdegsoil*(365-t))/Mperha_soil
  
  # Return results
  return(list(
    Admin_total = Admin_total,
    C_meat = C_meat, C_milk = C_milk, 
    C_soil = C_soil, Cn = Cn, 
    C_root = C_root, C_leaf = C_leaf, C_fruit = C_fruit, 
    MassCheck = MassCheck,
    C_soilpost = C_soilpost))
}
# 12.3 Loop 2
LoopResult <- Loop_calculation(C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root)
Admin_total <- LoopResult$Admin_total
C_meat <- LoopResult$C_meat
C_milk <- LoopResult$C_milk
C_soil <- LoopResult$C_soil
Cn <- LoopResult$Cn
C_root <- LoopResult$C_root
C_leaf <- LoopResult$C_leaf
C_fruit <- LoopResult$C_fruit
MassCheck <- LoopResult$MassCheck
C_soilpost <- LoopResult$C_soilpost

Loop_2 <- cbind(DCD*DDDA, Admin_total, C_milk, C_meat, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost)
print(Loop_2)
print(MassCheck)

Loop_2W <- cbind(Admin_total, C_manure, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost, C_milk, C_meat)
print(Loop_2W)
print(MassCheck)

# 12.4 Loop_3
LoopResult <- Loop_calculation(C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root)
Admin_total <- LoopResult$Admin_total
C_meat <- LoopResult$C_meat
C_milk <- LoopResult$C_milk
C_soil <- LoopResult$C_soil
Cn <- LoopResult$Cn
C_root <- LoopResult$C_root
C_leaf <- LoopResult$C_leaf
C_fruit <- LoopResult$C_fruit
MassCheck <- LoopResult$MassCheck
C_soilpost <- LoopResult$C_soilpost

Loop_3 <- cbind(DCD*DDDA, Admin_total, C_milk, C_meat, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost)
print(Loop_3)
print(MassCheck)

Loop_3W <- cbind(Admin_total, C_manure, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost, C_milk, C_meat)
print(Loop_3W)
print(MassCheck)

# 12.5 Loop_4
LoopResult <- Loop_calculation(C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root)
Admin_total <- LoopResult$Admin_total
C_meat <- LoopResult$C_meat
C_milk <- LoopResult$C_milk
C_soil <- LoopResult$C_soil
Cn <- LoopResult$Cn
C_root <- LoopResult$C_root
C_leaf <- LoopResult$C_leaf
C_fruit <- LoopResult$C_fruit
MassCheck <- LoopResult$MassCheck
C_soilpost <- LoopResult$C_soilpost

Loop_4 <- cbind(DCD*DDDA, Admin_total, C_milk, C_meat, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost)
print(Loop_4)
print(MassCheck)

Loop_4W <- cbind(Admin_total, C_manure, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost, C_milk, C_meat)
print(Loop_4W)
print(MassCheck)

# 12.6 Loop_5
LoopResult <- Loop_calculation(C_meat, C_milk, C_fruit, C_leaf, C_soilpost, C_root)
Admin_total <- LoopResult$Admin_total
C_meat <- LoopResult$C_meat
C_milk <- LoopResult$C_milk
C_soil <- LoopResult$C_soil
Cn <- LoopResult$Cn
C_root <- LoopResult$C_root
C_leaf <- LoopResult$C_leaf
C_fruit <- LoopResult$C_fruit
MassCheck <- LoopResult$MassCheck
C_soilpost <- LoopResult$C_soilpost

Loop_5 <- cbind(DCD*DDDA, Admin_total, C_milk, C_meat, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost)
print(Loop_5)
print(MassCheck)

Loop_5W <- cbind(Admin_total, C_manure, Cn, C_soil, C_root, C_leaf, C_fruit, C_soilpost, C_milk, C_meat)
print(Loop_5W)
print(MassCheck)

# 13. Result exporting
Nr_loop <- 5
Loop_rownames <- paste(rep(ABs, times = Nr_loop), 
                       rep(1:5, each = Nr_ABs), 
                       sep = "_")

# 13.1a CowCombined
Loop_CowCombined <- rbind(Loop_1, Loop_2, Loop_3, Loop_4, Loop_5)
Loop_CowCombined <- as.data.frame(Loop_CowCombined)
Loop_CowCombined$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_CowCombined) <- Loop_rownames
View(Loop_CowCombined)
# write.xlsx(Loop_CowCombined, "Result_CowCombined.xlsx", rowNames = TRUE)
# write.xlsx(Loop_CowCombined, "Result_CowCombined_worst.xlsx", rowNames = TRUE)
Loop_CowCombinedW <- rbind(Loop_1W, Loop_2W, Loop_3W, Loop_4W, Loop_5W)
Loop_CowCombinedW <- as.data.frame(Loop_CowCombinedW)
Loop_CowCombinedW$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_CowCombinedW) <- Loop_rownames
View(Loop_CowCombinedW)


# 13.1b CowSilage
Loop_CowSilage <- rbind(Loop_1, Loop_2, Loop_3, Loop_4, Loop_5)
Loop_CowSilage <- as.data.frame(Loop_CowSilage)
Loop_CowSilage$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_CowSilage) <- Loop_rownames
View(Loop_CowSilage)
# write.xlsx(Loop_CowSilage, "Result_CowSilage.xlsx", rowNames = TRUE)
# write.xlsx(Loop_CowSilage, "Result_CowSilage_worst.xlsx", rowNames = TRUE)
Loop_CowSilageW <- rbind(Loop_1W, Loop_2W, Loop_3W, Loop_4W, Loop_5W)
Loop_CowSilageW <- as.data.frame(Loop_CowSilageW)
Loop_CowSilageW$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_CowSilageW) <- Loop_rownames
View(Loop_CowSilageW)

# 13.1c Veal calves
Loop_Veal <- rbind(Loop_1, Loop_2, Loop_3, Loop_4, Loop_5)
Loop_Veal <- as.data.frame(Loop_Veal)
Loop_Veal$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_Veal) <- Loop_rownames
View(Loop_Veal)
# write.xlsx(Loop_Veal, "Result_CowVeal.xlsx", rowNames = TRUE)
# write.xlsx(Loop_Veal, "Result_CowVeal_worst.xlsx", rowNames = TRUE)
Loop_VealW <- rbind(Loop_1W, Loop_2W, Loop_3W, Loop_4W, Loop_5W)
Loop_VealW <- as.data.frame(Loop_VealW)
Loop_VealW$Year <- rep(1:5, each = Nr_ABs)
rownames(Loop_VealW) <- Loop_rownames
View(Loop_VealW)



# 9.2b M_root Plotting
color = c("blue","purple", "green4", "red")                          # Give a vector of colors that could distinguish different groups of data in plotting. terrain.colors(), heat.colors(), topo.colors()

par(mfrow = c(2,2))
plot(Input_M_root[,1], sens_C_root[,1],                              # Use data of Doxycycline as initial plotting setup for root concentration in PPCP model
     main = "Antibiotics concentration in root (PPCP model)", xlab = "Root biomass 0-50, changed by 0.01 (kg)", ylab="C_root(mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_root)-1){
  lines(Input_M_root[,i+1], sens_C_root[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 0.5)
abline(h = C_root, col = color, lty = 1)
axis(side =1, at = 0:10)
text(1, C_root[c(1,2,4)]+0.2, labels = round(C_root[c(1,2,4)], digits = 3), col = color[c(1,2,4)], cex = 1)
text(1, C_root[3]-0.15, labels = round(C_root[3], digits = 3), col = color[3], cex = 1)

plot(Input_M_root[,1], sens_C_leaf[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in PPCP model
     main = "Antibiotics concentration in leaf (PPCP model)", xlab = "Root biomass 0-50, changed by 0.01 (kg)", ylab="C_leaf(mg/kg)", 
     ylim = c(0,5), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_root)-1){
  lines(Input_M_root[,i+1], sens_C_leaf[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
abline(h = C_leaf, col = color, lty = 1)
text(1, C_leaf[c(1,4)]+0.2, labels = round(C_leaf[c(1,4)], digits = 3), col = color[c(1,4)], cex = 1)
text(1, C_leaf[2]+0.1, labels = round(C_leaf[2], digits = 3), col = color[2], cex = 1)
text(1, C_leaf[3]-0.1, labels = round(C_leaf[3], digits = 3), col = color[3], cex = 1)

plot(Input_M_root[,1], sens_C_rootS[,1],                             # Use data of Doxycycline as initial plotting setup for root concentration in standard model
     main = "Antibiotics concentration in root (standard model)", xlab = "Root biomass 0-50, changed by 0.01 (kg)", ylab="C_rootS(mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_root)-1){
  lines(Input_M_root[,i+1], sens_C_rootS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
abline(h = C_rootS, col = color, lty = 1)
text(1, C_rootS[c(1,2,4)]+0.2, labels = round(C_rootS[c(1,2,4)], digits = 3), col = color[c(1,2,4)], cex = 1)
text(1, C_rootS[3]+0.2, labels = round(C_rootS[3], digits = 3), col = color[3], cex = 1)

plot(Input_M_root[,1], sens_C_leafS[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in standard model
     main = "Antibiotics concentration in leaf (standard model)", xlab = "Root biomass 0-50, changed by 0.01 (kg)", ylab="C_leafS(mg/kg)", 
     ylim = c(0,5), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_root)-1){
  lines(Input_M_root[,i+1], sens_C_leafS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
abline(h = C_leafS, col = color, lty = 1)
text(1, C_leafS[c(1:4)]+0.1, labels = round(C_leafS[c(1:4)], digits = 3), col = color[c(1:4)], cex = 1)

# 9.3a M_leaf setup
Input_M_leaf <- matrix(NA, nrow = Nr_sensitivity, ncol = length(ABs))
colnames(Input_M_leaf) <- ABs

for (i in 1:Nr_sensitivity){                                                    # M_leaf value setup (0.01-5.00, by 0.01).
  Input_M_leaf[i,] <- i/100
} 

for (i in 1:Nr_sensitivity){
  sens_root2leafS[i,] <- (Q_leaf*C_xyS)/Input_M_leaf[i,]
  sens_air2leafS[i,] <- (C_air*g*area_leaf)/Input_M_leaf[i,]
  sens_loss_leafS[i,] <- (g*area_leaf*ρ_leaf/KlaS)/Input_M_leaf[i,] + k_leaf    
    sens_C_leafS[i,] <- C_leaf0*exp(-sens_loss_leafS[i,]*t) + ((sens_root2leafS[i,] + sens_air2leafS[i,])/sens_loss_leafS[i,])*(1-exp(-sens_loss_leafS[i,]*t))
  sens_loss2air_leaf[i,] <- (g*area_leaf*ρ_leaf/Kla)/Input_M_leaf[i,]
  sens_loss2root_leaf[i,] <- KPhloL*Qphlo_leaf/Input_M_leaf[i,] 
  sens_loss2fruit_leaf[i,] <- KPhloL*Qphlo_fruit/Input_M_leaf[i,] 
  sens_root2leaf[i,] <- Q_leaf*KXyR/Input_M_leaf[i,]
  sens_airuptake_leaf[i,] <- g*area_leaf*C_air/Input_M_leaf[i,]
    sens_loss_leaf[i,] <- sens_loss2air_leaf[i,] + sens_loss2root_leaf[i,] + sens_loss2fruit_leaf[i,] + k_leaf
      sens_C_root[i,] <- (soil2root*sens_loss_leaf[i,]*C_soil + sens_airuptake_leaf[i,]*leaf2root)/(loss_root*sens_loss_leaf[i,] - leaf2root*sens_root2leaf[i,])
        sens_C_leaf[i,] <- (sens_root2leaf[i,]*sens_C_root[i,] + sens_airuptake_leaf[i,])/sens_loss_leaf[i,]  
}
sens_C_rootS <- matrix(rep(C_rootS, 500), nrow = 500, byrow = TRUE)
colnames(sens_C_rootS) <- ABs

# 9.3b M_leaf plotting 
par(mfrow = c(2,2))
plot(Input_M_leaf[,1], sens_C_root[,1],                              # Use data of Doxycycline as initial plotting setup for root concentration in PPCP model
     main = "Antibiotics concentration in root (PPCP model)", xlab = "Leaf biomass 0-50, changed by 0.01 (kg)", ylab="C_root(mg/kg)", 
     ylim = c(0,5), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_leaf)-1){
  lines(Input_M_leaf[,i+1], sens_C_root[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 1)
text(1, C_root[c(1,2,4)]+0.2, labels = round(C_root[c(1,2,4)], digits = 3), col = color[c(1,2,4)], cex = 1)
text(1, C_root[3]-0.15, labels = round(C_root[3], digits = 3), col = color[3], cex = 1)

plot(Input_M_leaf[,1], sens_C_leaf[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in PPCP model
     main = "Antibiotics concentration in leaf (PPCP model)", xlab = "Leaf biomass 0-50, changed by 0.01 (kg)", ylab="C_leaf(mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_leaf)-1){
  lines(Input_M_leaf[,i+1], sens_C_leaf[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
abline(h = C_leaf, col = color, lty = 1)
text(1, C_leaf[c(1,4)]+0.2, labels = round(C_leaf[c(1,4)], digits = 3), col = color[c(1,4)], cex = 1)
text(1, C_leaf[2]-0.1, labels = round(C_leaf[2], digits = 3), col = color[2], cex = 1)
text(1, C_leaf[3]-0.2, labels = round(C_leaf[3], digits = 3), col = color[3], cex = 1)

plot(Input_M_leaf[,1], sens_C_rootS[,1],                             # Use data of Doxycycline as initial plotting setup for root concentration in standard model
     main = "Antibiotics concentration in root (standard model)", xlab = "Leaf biomass 0-50, changed by 0.01 (kg)", ylab="C_rootS(mg/kg)", 
     ylim = c(0,5), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_leaf)-1){
  lines(Input_M_leaf[,i+1], sens_C_rootS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
text(1, C_rootS+0.1, labels = round(C_rootS, digits = 3), col = color, cex = 1)

plot(Input_M_leaf[,1], sens_C_leafS[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in standard model
     main = "Antibiotics concentration in leaf (standard model)", xlab = "Leaf biomass 0-50, changed by 0.01 (kg)", ylab="C_leafS(mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_leaf)-1){
  lines(Input_M_leaf[,i+1], sens_C_leafS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05))
abline(h = C_leafS, col = color, lty = 1)
text(1, C_leafS[c(1:4)]+0.13, labels = round(C_leafS[c(1:4)], digits = 3), col = color[c(1:4)], cex = 1)

# 9.4a Q_root setup (not yet considering the relationship between Q_root and Q_leaf)
Input_Q_root <- matrix(NA, nrow = Nr_sensitivity, ncol = length(ABs))
colnames(Input_Q_root) <- ABs

for (i in 1:Nr_sensitivity){                                                    # M_root value setup (0.01-5.00, by 0.01).
  Input_Q_root[i,] <- i/100
} 

for (i in 1:Nr_sensitivity){
  sens_C_rootS[i,] <- Cw_soil*(F_rootS*Input_Q_root[i,])/((Input_Q_root[i,]/KrwS) + k_root*M_root)
    sens_C_xyS[i,] <- sens_C_rootS[i,]/KrwS 
      sens_root2leafS[i,] <- (Q_leaf*sens_C_xyS[i,])/M_leaf
        sens_C_leafS[i,] <- C_leaf0*exp(-loss_leafS*t) + ((sens_root2leafS[i,] + air2leafS)/loss_leafS)*(1-exp(-loss_leafS*t))
  sens_soil2root[i,] <- F_root*Input_Q_root[i,]*KXyW*Kws/M_root
  sens_loss_root[i,] <- k_root + Input_Q_root[i,]*KXyR/M_root
    sens_C_root[i,] <- (sens_soil2root[i,]*loss_leaf*C_soil + airuptake_leaf*leaf2root)/(sens_loss_root[i,]*loss_leaf - leaf2root*root2leaf)
      sens_C_leaf[i,] <- (root2leaf*sens_C_root[i,] + airuptake_leaf)/loss_leaf  
}

# 9.4b Q_root Plotting
par(mfrow = c(2,2))
plot(Input_Q_root[,1], sens_C_root[,1],                              # Use data of Doxycycline as initial plotting setup for root concentration in PPCP model
     main = "Antibiotics concentration in root (PPCP model)", xlab = "Root transpiration stream 0-50, changed by 0.01 (L/day)", ylab="C_root (mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_Q_root)-1){
  lines(Input_Q_root[,i+1], sens_C_root[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 0.6)
abline(h = sens_C_root[100,], col = color, lty = 1)                  # the row 100 represent the predicted values when Q_root = 1 (the input of original paper)
text(1, sens_C_root[100,-3]+0.2, labels = round(C_root[c(1,2,4)], digits = 3), col = color[c(1,2,4)], cex = 1)
text(1, C_root[3]-0.15, labels = round(C_root[3], digits = 3), col = color[3], cex = 1)

plot(Input_Q_root[,1], sens_C_leaf[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in PPCP model
     main = "Antibiotics concentration in leaf (PPCP model)", xlab = "Root transpiration stream 0-50, changed by 0.01 (L/day)", ylab="C_leaf (mg/kg)", 
     ylim = c(0,5), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_M_root)-1){
  lines(Input_Q_root[,i+1], sens_C_leaf[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 0.6)
abline(h = sens_C_leaf[100,], col = color, lty = 1)
text(1, sens_C_leaf[100,]+0.05, labels = round(sens_C_leaf[100,], digits = 3), col = color, cex = 1)

plot(Input_Q_root[,1], sens_C_rootS[,1],                             # Use data of Doxycycline as initial plotting setup for root concentration in standard model
     main = "Antibiotics concentration in root (standard model)", xlab = "Root transpiration stream 0-50, changed by 0.01 (L/day)", ylab="C_rootS (mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_Q_root)-1){
  lines(Input_Q_root[,i+1], sens_C_rootS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 0.6)
abline(h = sens_C_rootS[100,], col = color, lty = 1)
text(1, sens_C_rootS[100,]+0.1, labels = round(sens_C_rootS[100,], digits = 3), col = color, cex = 1)

plot(Input_Q_root[,1], sens_C_leafS[,1],                              # Use data of Doxycycline as initial plotting setup for leaf concentration in standard model
     main = "Antibiotics concentration in leaf (standard model)", xlab = "Root transpiration stream 0-50, changed by 0.01 (L/day)", ylab="C_leafS (mg/kg)", 
     ylim = c(0,10), col = color[1], type = "l", lwd = 2, lty = 2)
for (i in 1: ncol(Input_Q_root)-1){
  lines(Input_Q_root[,i+1], sens_C_leafS[,i+1], col = color[i+1], lwd = 2, lty = 2)
}
legend("topright", legend = ABs, col = color, lwd = 2, lty = 2, inset = c(0.05, 0.05), cex = 0.6)
abline(h = sens_C_leafS[100,], col = color, lty = 1)
text(1, sens_C_leafS[100,]+0.1, labels = round(sens_C_leafS[100,], digits = 3), col = color, cex = 1)

#input_0_30 <- c("C_soil","ρDry_soil","pH_soil","Q_root","M_root", "ρ_leaf", "M_leaf", "t")
#input_chemical <- c("lgKow_n","pKa","Kaw", "Khsa")
#input_percent <- c("OC","Vwater_soil", "Vwater_root", "Lipid_root", "Pro_root")

