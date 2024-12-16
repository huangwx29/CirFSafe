# 1.Project Background 
# File owner: Weixin Huang
# Creation data: 20240902
# Version number: 20241114
# Update content: Update plots with latest predictions; Add units for one-off scenario plotting.

# 2. Manual of this R file
# Plotting the simulated results of 3 animal-diet on the 8 Antimicrobials under 2 exposure scenarios. 
# Instruction: 

# 3. Package loading
library(readxl)
library(dplyr)

# 4. Import parameters: choose CowSOIL file to import data. This file has updated degradation values in soil
# 4.1a Constant exposure scenario
Result_Constant <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/ResultTable_Constant1.xlsx")
Result_Constant <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/ResultTable_Constant1.xlsx")

Result_Constant <- as.data.frame(Result_Constant)
Result_Constant$Scenario <- factor(Result_Constant$Scenario, levels = unique(Result_Constant$Scenario))
Result_Constant$AMs <- factor(Result_Constant$AMs, levels = c("Doxycycline", "Tetracycline", "Flumequine", "Lincomycin", "Trimethoprim"))
str(Result_Constant)
summary(Result_Constant)
View(Result_Constant)

# 4.1b One-off exposure scenario
Result_OneOff <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/ResultTable_OneOff.xlsx")
Result_OneOff <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/ResultTable_OneOff.xlsx")

Result_OneOff <- as.data.frame(Result_OneOff)
Result_OneOff$Scenario <- factor(Result_OneOff$Scenario, levels = unique(Result_OneOff$Scenario))
matrix <- unique(Result_OneOff$Matrix)
matrix_c <- matrix[-c(1,5,7)]                                                   # remove C_manure, Q_admin(different unit), and choose right food products
matrix_v <- matrix[-c(1,5,6)] 
#matrix_c <- as.factor(matrix_c)
#matrix_v <- as.factor(matrix_v)
#Result_OneOff$Matrix <- factor(Result_OneOff$Matrix, levels = matrix)
summary(Result_OneOff)
View(Result_OneOff)

# 4.2a filter dataset as per animal (diet) - constant exposure
CC_Constant <- Result_Constant[Result_Constant$Scenario == "CC",]
CC_ConstantA <- filter(CC_Constant, Year != 3)
CC_ConstantA <- filter(CC_ConstantA, Year != 4)
CS_Constant <- Result_Constant[Result_Constant$Scenario == "CS",]
CS_ConstantA <- filter(CS_Constant, Year != 3)
CS_ConstantA <- filter(CS_ConstantA, Year != 4)
VC_Constant <- Result_Constant[Result_Constant$Scenario == "VC",]
VC_ConstantA <- filter(VC_Constant, Year != 3)
VC_ConstantA <- filter(VC_ConstantA, Year != 4)

# 4.2b filter dataset as per animal (diet) - one-off exposure
CC_OneOff <- Result_OneOff[Result_OneOff$Scenario == "CC",]
CS_OneOff <- Result_OneOff[Result_OneOff$Scenario == "CS",]
VC_OneOff <- Result_OneOff[Result_OneOff$Scenario == "VC",]
VC_OneOff <- VC_OneOff[,-7]                                                     # exclude NA

# 4.3 Sensitivity analysis & dataset filtering
Result_SA <- read_excel("C:/Users/huang067/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/SA.xlsx")
Result_SA <- read_excel("C:/Users/whuang29/OneDrive - Wageningen University & Research/Huang067/Chapter 2/R/SA.xlsx")
Result_SA <- as.data.frame(Result_SA)

Result_SA_Admin <- Result_SA[Result_SA$Variables == "Admin_total",]
Result_SA_milk <- Result_SA[Result_SA$Variables == "C_milk",]

# 5. Plotting      
# 5.1. Constant Exposure
# 5.1.1. Global parameters
# Colors representing different AMs
color <- c("royalblue1", "slateblue1","sandybrown", "seagreen", "lightpink")     # Give a vector of colors that could distinguish different groups of data in plotting. terrain.colors(), heat.colors(), topo.colors()
color_p <- c("royalblue3" , "slateblue3", "tan3", "seagreen", "lightpink3")     # Give a vector of colors that could distinguish different groups of data in plotting. terrain.colors(), heat.colors(), topo.colors()
AMs <- sort(unique(CC_Constant$AMs))                                            # Sort the sequence of AMs for plotting 
# Define bar width
bar_width <- 0.6/length(AMs)
# Define bar width, exlcusively for grain plotting
bar_width_G <- 0.8/length(AMs)
# Create a mapping for x-axis positions
x_position <- rep(c(0.75, 1.5, 2.5), each = 5)
x_position_G <- rep(c(1.1, 2.2, 3.5), each = 5)

# 5.1.2. Aggregate for CONSTANT exposure scenario - Concentration in FOOD PRODUCTS
# Define layout of plotting
par(mfrow = c(2,1), oma = c(8, 4, 10, 4), mar = c(2,8,2,6), las = 1)
# 5.1.2a Plot Dairy cow scenario
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position)/2 + 0.1, 
              max(x_position) + min(x_position)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_ConstantA$C_milk)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_ConstantA$C_milk)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(CC_ConstantA$C_milk)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.09, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in milk (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.5)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position) - min(x_position)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0, 
       xright = unique(x_position) - min(x_position)/2 + 0.1 + i*bar_width, 
       ytop = AMs_data$C_milk, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  text(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width,
       AMs_data$C_milk,
       label = sprintf("%.3f", AMs_data$C_milk),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 1.5,                                                            # adjust the value to move labels up or down
       cex = 1.05,
       col = "black"
  )
}
# Keep plotting for the secondary y-axis before plotting the primary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position, x_position, type = "n",
     xlim = c(min(x_position)/2 + 0.1, max(x_position) + 
                min(x_position)/2 - 0.1), 
     ylim = c(0, max(CC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Circular farming with dairy cows (CC & CS)",
     xaxt = "n", 
     cex.main = 1.5, cex.axis = 1.5,
     mgp = c(0, 1, 0)
)
text(x = par("usr")[1]*0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add custom x-axis
axis(1, at = x_position, labels = CC_ConstantA$Year, cex.axis = 1.5, mgp = c(3,1,0))
# Add symbol
text(2, 0, "//", srt = 0, adj = c(0, 1), cex = 2, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  points(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width, 
         AMs_data$AnnualDose, type = "p",
         lwd = 1.5, pch = 21, cex = 1.5,
         bg = color[i], col = "grey27"
  )
}

# 5.1.2b. Plot Veal calves scenario
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position)/2 + 0.1, 
              max(x_position) + min(x_position)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_ConstantA$C_meat)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_ConstantA$C_meat)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(VC_ConstantA$C_meat)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.09, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in meat (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.5)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position) - min(x_position)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0, 
       xright = unique(x_position) - min(x_position)/2 + 0.1 + i*bar_width, 
       ytop = AMs_data$C_meat, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  text(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width,
       AMs_data$C_meat,
       label = sprintf("%.3f", AMs_data$C_meat),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 1.5,                                                            # adjust the value to move labels up or down
       cex = 1.05,
       col = "black"
  )
}
# Keep plotting for the secondary y-axis before plotting the primary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position, x_position, type = "n",
     xlim = c(min(x_position)/2 + 0.1, max(x_position) + 
                min(x_position)/2 - 0.1), 
     ylim = c(0, max(VC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Circular farming with veal calves (VC)",
     xaxt = "n", 
     cex.main = 1.5, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg veal weights)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add custom x-axis
axis(1, at = x_position, labels = VC_ConstantA$Year, cex.axis = 1.5, mgp = c(3,1,0))
# Add symbol
text(2, 0, "//", srt = 0, adj = c(0, 1), cex = 2, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  points(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width, 
         AMs_data$AnnualDose, type = "p",
         lwd = 1.5, pch = 21, cex = 1.5,
         bg = color[i], col = "grey27"
  )
}

mtext("AM exposure and residuals in food products", outer = TRUE, cex = 2, line = 1)
mtext("Year", outer = TRUE, side = 1, cex = 1.5, line = 1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = as.character(AMs), col = "grey27",
       pch = rep(22, length(AMs)), pt.bg = color,                             # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       pt.cex = 2.5,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.2,                                                                 # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0, 0),
       #bty = "n",                                                               # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                               # this allows plotting outside the figure region
       x.intersp = 0.8,
       y.intersp = 0.5,
       text.width = 0.2
)
# 5.1.3. Aggregate for CONSTANT exposure scenario - Concentration in MAIZE GRAINS
# Define layout of plotting
par(mfrow = c(2,1), oma = c(8, 4, 10, 4), mar = c(2, 8, 2, 7), las = 1)
# 5.1.3a Plot Dairy cow system
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position)/2 + 0.1, 
              max(x_position) + min(x_position)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_ConstantA$C_fruit)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_ConstantA$C_fruit)*1.6)), 
     labels = sprintf("%.4f",pretty(c(0, max(CC_ConstantA$C_fruit)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.105, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in grains (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.5)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position) - min(x_position)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0, 
       xright = unique(x_position) - min(x_position)/2 + 0.1 + i*bar_width, 
       ytop = AMs_data$C_fruit, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  text(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width,
       AMs_data$C_fruit,
       label = sprintf("%.4f", AMs_data$C_fruit),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 0.9,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position, x_position, type = "n",
     xlim = c(min(x_position)/2 + 0.1, max(x_position) + 
                min(x_position)/2 - 0.1), 
     ylim = c(0, max(CC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Circular farming with dairy cows (CC & CS)",
     xaxt = "n", 
     cex.main = 1.5, cex.axis = 1.5,
     mgp = c(0, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add custom x-axis
axis(1, at = x_position, labels = CC_ConstantA$Year, cex.axis = 1.5, mgp = c(3,1,0))
# Add symbol
text(2, 0, "//", srt = 0, adj = c(0, 1), cex = 2, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  points(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width, 
         AMs_data$AnnualDose, type = "p",
         lwd = 1.5, pch = 21, cex = 1.5,
         bg = color[i], col = "grey27"
  )
}
# 5.1.3b. Plot Veal calves scenario
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position)/2 + 0.1, 
              max(x_position) + min(x_position)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_ConstantA$C_fruit)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_ConstantA$C_fruit)*1.6)), 
     labels = sprintf("%.4f",pretty(c(0, max(VC_ConstantA$C_fruit)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.105, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in grains (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.5)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position) - min(x_position)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0, 
       xright = unique(x_position) - min(x_position)/2 + 0.1 + i*bar_width, 
       ytop = AMs_data$C_fruit, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  text(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width,
       AMs_data$C_fruit,
       label = sprintf("%.4f", AMs_data$C_fruit),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.8,                                                            # adjust the value to move labels up or down
       cex = 0.9,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position, x_position, type = "n",
     xlim = c(min(x_position)/2 + 0.1, max(x_position) + 
                min(x_position)/2 - 0.1), 
     ylim = c(0, max(VC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Circular farming with veal calves (VC)",
     xaxt = "n", 
     cex.main = 1.5, cex.axis = 1.5,
     mgp = c(0, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg veal weights)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add custom x-axis
axis(1, at = x_position, labels = VC_ConstantA$Year, cex.axis = 1.5, mgp = c(3,1,0))
# Add symbol
text(2, 0, "//", srt = 0, adj = c(0, 1), cex = 2, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  points(unique(x_position) - min(x_position)/2 + 0.1 + (i-0.5)*bar_width, 
         AMs_data$AnnualDose, type = "p",
         lwd = 1.5, pch = 21, cex = 1.5,
         bg = color[i], col = "grey27"
  )
}
mtext("AM exposure and residuals in maize grains", outer = TRUE, cex = 2, line = 1)
mtext("Year", outer = TRUE, side = 1, cex = 1.5, line = 1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = as.character(AMs), col = "grey27",
       pch = rep(22, length(AMs)), pt.bg = color,                               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       pt.cex = 2.5,                                                            # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.2,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0, 0),
       #bty = "n",                                                               # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                               # this allows plotting outside the figure region
       x.intersp = 0.8,
       y.intersp = 0.5,
       text.width = 0.2
)

# 5.1.4. Aggregate for CONSTANT exposure scenario - Concentration in MAIZE SHOOTS
# Define layout of plotting
par(mfrow = c(3,1), oma = c(4, 4, 10, 4), mar = c(4, 8, 4, 8), las = 1)
# 5.1.4a Plot Dairy cow with combined diet
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_ConstantA$C_leaf)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_ConstantA$C_leaf)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(CC_ConstantA$C_leaf)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in shoots (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_leaf, 
       col = color[i], 
       border = "darkgrey")
}

for(i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_leaf,
       label = sprintf("%.4f", AMs_data$C_leaf),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the secondary y-axis before plotting the primary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(CC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Dairy cows fed with combined diet (CC)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = CC_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
# 5.1.4b Plot Dairy cow with silage only diet
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CS_ConstantA$C_leaf)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CS_ConstantA$C_leaf)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(CS_ConstantA$C_leaf)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in shoots (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_leaf, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_leaf,
       label = sprintf("%.4f", AMs_data$C_leaf),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(CS_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Dairy cows fed with silage-only diet (CS)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = CS_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
# 5.1.4c Plot Veal calves scenario
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_ConstantA$C_leaf)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_ConstantA$C_leaf)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(VC_ConstantA$C_leaf)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in shoots (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_leaf, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_leaf,
       label = sprintf("%.3f", AMs_data$C_leaf),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(VC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Veal calves fed with combined diet (VC)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg veal weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = VC_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
mtext("AM exposure and residuals in maize shoots", outer = TRUE, cex = 2, line = 0)
mtext("Year", outer = TRUE, side = 1, cex = 1.5, line = 0)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = as.character(AMs), col = "grey27",
       pch = rep(22, length(AMs)), pt.bg = color,                               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       pt.cex = 2.5,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.5,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0, 0),
       #bty = "n",                                                               # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                               # this allows plotting outside the figure region
       x.intersp = 0.8,
       y.intersp = 0.5,
       text.width = 0.2
)

# 5.1.5. Aggregate for CONSTANT exposure scenario - Concentration in soil
# Define layout of plotting
par(mfrow = c(3,1), oma = c(4, 4, 10, 4), mar = c(4,8,4,8), las = 1)
# 5.1.5a Plot Dairy cow combined diet
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_ConstantA$C_soil)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_ConstantA$C_soil)*1.6)), 
     labels = sprintf("%.4f",pretty(c(0, max(CC_ConstantA$C_soil)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in soil (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_soil, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_soil,
       label = sprintf("%.4f", AMs_data$C_soil),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(CC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Dairy cows fed with combined diet (CC)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = CC_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CC_ConstantA[CC_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
# 5.1.5b Plot Dairy cow silage only diet
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(CS_ConstantA$C_soil)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CS_ConstantA$C_soil)*1.6)), 
     labels = sprintf("%.4f",pretty(c(0, max(CS_ConstantA$C_soil)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in soil (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_soil, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_soil,
       label = sprintf("%.4f", AMs_data$C_soil),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(CS_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Dairy cows fed with silage-only diet (CS)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg cow weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = CS_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- CS_ConstantA[CS_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
# 5.1.5c. Plot Veal calves scenario
# Plot secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(x_position_G)/2 + 0.1, 
              max(x_position_G) + min(x_position_G)/2 - 0.1),                       # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_ConstantA$C_soil)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_ConstantA$C_soil)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(VC_ConstantA$C_soil)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.07, 
     y = mean(par("usr")[3:4]), 
     labels = "AM concentrations in soil (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 2)
# Plot bar charts
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  rect(xleft = unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-1)*bar_width_G, 
       ybottom = 0, 
       xright = unique(x_position_G) - min(x_position_G)/2 + 0.1 + i*bar_width_G, 
       ytop = AMs_data$C_soil, 
       col = color[i], 
       border = "darkgrey")
}
for(i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  text(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G,
       AMs_data$C_soil,
       label = sprintf("%.4f", AMs_data$C_soil),                                # format to 2 decimal places
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.2,
       col = "black"
  )
}
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Set up the plot area and y-axis
plot(x_position_G, x_position_G, type = "n",
     xlim = c(min(x_position_G)/2 + 0.1, max(x_position_G) + 
                min(x_position_G)/2 - 0.1), 
     ylim = c(0, max(VC_ConstantA$AnnualDose)),
     xlab = "", ylab = "", 
     main = "Veal calves fed with combined diet (VC)",
     xaxt = "n", 
     cex.main = 2, cex.axis = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.5, 
     y = mean(par("usr")[3:4]), 
     labels = "Annual AM dose (mg/kg veal weights)", 
     srt = 90, xpd = TRUE, cex = 2)
# Add custom x-axis
axis(1, at = x_position_G, labels = VC_ConstantA$Year, cex.axis = 2, mgp = c(3,1.5,0))
# Add symbol
text(min(x_position_G)*2 + (max(x_position_G)-min(x_position_G)*2)/2, 
     3, "//", srt = 0, adj = c(0, 1), cex = 3, xpd = TRUE)
# Add points describing annual dose
for (i in 1:length(AMs)){
  AMs_data <- VC_ConstantA[VC_ConstantA$AMs == AMs[i],]
  points(unique(x_position_G) - min(x_position_G)/2 + 0.1 + (i-0.5)*bar_width_G, 
         AMs_data$AnnualDose, type = "p",
         lwd = 2, pch = 21, cex = 2,
         bg = color[i], col = "grey27"
  )
}
mtext("AM exposure and residuals in soil", outer = TRUE, cex = 2, line = 0)
mtext("Year", outer = TRUE, side = 1, cex = 1.5, line = 0)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = as.character(AMs), col = "grey27",
       pch = rep(22, length(AMs)), pt.bg = color,                               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       pt.cex = 2.5,                                                            # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.5,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0, 0),
       #bty = "n",                                                               # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                              # this allows plotting outside the figure region
       x.intersp = 0.8,
       y.intersp = 0.5,
       text.width = 0.2
)


# 5.2. One-off Exposure
# 5.2.1. global parameters
# soil, root, shoot, fruit, admin, milk/meat
comp <- c("concentration in manure", 
  "concentration in soil", "concentration in shoots",
  "concentration in grains", "concentration in milk", "concentration in meat"
)
comp_c <- comp[-c(1,6)]
comp_v <- comp[-c(1,5)]
color_OneOff <- 
  c("red2",
    "lightsalmon4", "darkolivegreen2", 
    "darkgoldenrod1", "azure3","lightpink3"
)
color_c <- color_OneOff[-6]
color_v <- color_OneOff[-5]
AMs_c <- c("Doxycycline", "Tetracycline", "Flumequine", "Lincomycin")
AMs_v <- c("Doxycycline", "Tetracycline", "Flumequine")
bar_width <- 0.85/length(comp_c)
CC_OneOff <- CC_OneOff[CC_OneOff$Year < 4, ]                                   # keep only year 1-3
CS_OneOff <- CS_OneOff[CS_OneOff$Year < 4, ] 
VC_OneOff <- VC_OneOff[VC_OneOff$Year < 4, ] 
CC_OneOffA <- CC_OneOff[CC_OneOff$Matrix != matrix[1], ]                        # excluding values of C_manure
CS_OneOffA <- CS_OneOff[CS_OneOff$Matrix != matrix[1], ] 
VC_OneOffA <- VC_OneOff[VC_OneOff$Matrix != matrix[1], ] 

# 5.2.2. Aggregate for one-off exposure scenario - Doxycycline
par(mfrow = c(2,1), oma = c(3, 3, 11, 4), mar = c(4,6,3,8) + 0.5, las = 1)
# 5.2.2a. Plot CC
# Set up the plot area and y-axis
plot(unique(CC_OneOff$Year), 
     CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[1]], 
     type = "n",
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[1]]*1.1)),     
     xlab = "", ylab = "", 
     main = "Circular farming with dairy cows (CC & CS)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(CC_OneOff$Year), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add points describing manure concentration
lines(unique(CC_OneOff$Year), 
      CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[1]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[1]]*0.95, 
     labels = sprintf("%.0f", CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[1]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_OneOffA$Doxycycline)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_OneOffA$Doxycycline)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(CC_OneOffA$Doxycycline)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i], ]
  rect(xleft = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Doxycycline,
       col = color_c[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i],]
  text(Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Doxycycline,
       label = sprintf("%.2f", Matrix_data$Doxycycline),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
# 5.2.2b. Plot VC
plot(unique(VC_OneOff$Year), 
     VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[1]], 
     type = "n",
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[1]]*1.1)),     
     xlab = "", ylab = "", 
     main = "Circular farming with veal calves (VC)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(VC_OneOff$Year), cex.axis = 1.5)
# Add points describing manure concentration
lines(unique(VC_OneOff$Year), 
      VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[1]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[1]]*0.95, 
     labels = sprintf("%.0f", VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[1]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_OneOffA$Doxycycline)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_OneOffA$Doxycycline)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(VC_OneOffA$Doxycycline)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i], ]
  rect(xleft = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Doxycycline,
       col = color_v[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i],]
  text(Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Doxycycline,
       label = sprintf("%.2f", Matrix_data$Doxycycline),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
mtext("Doxycycline (DOX) concentrations in farming compartments and products", outer = TRUE, cex = 2, line = 0.1)
mtext("Year", outer = TRUE, side = 1, cex = 1.7, line = -1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = comp, col = color_OneOff,
       pch = c(7, rep(22, length(comp)-1)), pt.bg = color_OneOff,               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       lty = c(6, rep(NA, length(comp)-1)), 
       lwd = c(3, rep(1, length(comp)-1)),
       pt.cex = 2,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.4,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0.01, 0.01),
       #bty = "n",                                                              # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                              # this allows plotting outside the figure region
       x.intersp = 0.2,
       y.intersp = 0.7,
       text.width = 0.4
)

# 5.2.3. Aggregate for one-off exposure scenario - Tetracycline
par(mfrow = c(2,1), oma = c(3, 3, 11, 4), mar = c(4,6,3,8) + 0.5, las = 1)
# 5.2.3a. Plot CC
# Set up the plot area and y-axis
plot(unique(CC_OneOff$Year), 
     CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[2]], 
     type = "n",
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[2]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Circular farming with dairy cows (CC & CS)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(CC_OneOff$Year), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add points describing manure concentration
lines(unique(CC_OneOff$Year), 
      CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[2]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[2]]*0.95, 
     labels = sprintf("%.0f", CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[2]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_OneOffA$Tetracycline)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_OneOffA$Tetracycline)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(CC_OneOffA$Tetracycline)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i], ]
  rect(xleft = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Tetracycline,
       col = color_c[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i],]
  text(Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Tetracycline,
       label = sprintf("%.2f", Matrix_data$Tetracycline),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
# 5.2.3b. Plot VC
plot(unique(VC_OneOff$Year), 
     VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[2]], 
     type = "n",
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[2]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Circular farming with veal calves (VC)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(VC_OneOff$Year), cex.axis = 1.5)
# Add points describing manure concentration
lines(unique(VC_OneOff$Year), 
      VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[2]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[2]]*0.95, 
     labels = sprintf("%.1f", VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[2]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_OneOffA$Tetracycline)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_OneOffA$Tetracycline)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(VC_OneOffA$Tetracycline)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i], ]
  rect(xleft = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Tetracycline,
       col = color_v[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i],]
  text(Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Tetracycline,
       label = sprintf("%.2f", Matrix_data$Tetracycline),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
mtext("Tetracycline (TC) concentrations in farming compartments and products", outer = TRUE, cex = 2, line = 0.1)
mtext("Year", outer = TRUE, side = 1, cex = 1.7, line = -1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = comp, col = color_OneOff,
       pch = c(7, rep(22, length(comp)-1)), pt.bg = color_OneOff,               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       lty = c(6, rep(NA, length(comp)-1)), 
       lwd = c(3, rep(1, length(comp)-1)),
       pt.cex = 2,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.4,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0.01, 0.01),
       #bty = "n",                                                              # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                              # this allows plotting outside the figure region
       x.intersp = 0.2,
       y.intersp = 0.7,
       text.width = 0.4
)


# 5.2.4. Aggregate for one-off exposure scenario - Flumequine
par(mfrow = c(2,1), oma = c(3, 3, 11, 4), mar = c(4,6,3,8) + 0.5, las = 1)
# 5.2.4a. Plot CC
# Set up the plot area and y-axis
plot(unique(CC_OneOff$Year), 
     CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[3]], 
     type = "n",
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[3]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Circular farming with dairy cows (CC & CS)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(CC_OneOff$Year), cex.axis = 1.5, mgp = c(3, 1.5, 0))
# Add points describing manure concentration
lines(unique(CC_OneOff$Year), 
      CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[3]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[3]]*0.95, 
     labels = sprintf("%.0f", CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[3]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_OneOffA$Flumequine)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_OneOffA$Flumequine)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(CC_OneOffA$Flumequine)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i], ]
  rect(xleft = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Flumequine,
       col = color_c[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i],]
  text(Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Flumequine,
       label = sprintf("%.2f", Matrix_data$Flumequine),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
# 5.2.4b. Plot VC
plot(unique(VC_OneOff$Year), 
     VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[3]], 
     type = "n",
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[3]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Circular farming with veal calves (VC)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(VC_OneOff$Year), cex.axis = 1.5)
# Add points describing manure concentration
lines(unique(VC_OneOff$Year), 
      VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[3]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[3]]*0.95, 
     labels = sprintf("%.0f", VC_OneOff[VC_OneOff$Matrix == matrix[1], AMs_v[3]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(VC_OneOff$Year)/2 + 0.1, 
              max(VC_OneOff$Year) + min(VC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(VC_OneOffA$Flumequine)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(VC_OneOffA$Flumequine)*1.6)), 
     labels = sprintf("%.2f",pretty(c(0, max(VC_OneOffA$Flumequine)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i], ]
  rect(xleft = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Flumequine,
       col = color_v[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_v)){
  Matrix_data <- VC_OneOff[VC_OneOff$Matrix == matrix_v[i],]
  text(Matrix_data$Year - min(VC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Flumequine,
       label = sprintf("%.2f", Matrix_data$Flumequine),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
mtext("Flumequine (FLU) concentrations in farming compartments and products", outer = TRUE, cex = 2, line = 0.1)
mtext("Year", outer = TRUE, side = 1, cex = 1.7, line = -1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = comp, col = color_OneOff,
       pch = c(7, rep(22, length(comp)-1)), pt.bg = color_OneOff,               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       lty = c(6, rep(NA, length(comp)-1)), 
       lwd = c(3, rep(1, length(comp)-1)),
       pt.cex = 2,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.4,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0.01, 0.01),
       #bty = "n",                                                              # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                              # this allows plotting outside the figure region
       x.intersp = 0.2,
       y.intersp = 0.7,
       text.width = 0.4
)

# 5.2.5. Aggregate for one-off exposure scenario - Lincomycin
par(mfrow = c(2,1), oma = c(3, 3, 11, 4), mar = c(4,6,3,8) + 0.5, las = 1)
# 5.2.5a. Plot CC
# Set up the plot area and y-axis
plot(unique(CC_OneOff$Year), 
     CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[4]], 
     type = "n",
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[4]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Dairy cows with combined diet (CC)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)

# Add custom x-axis
axis(1, at = unique(CC_OneOff$Year), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add points describing manure concentration
lines(unique(CC_OneOff$Year), 
      CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[4]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[4]]*0.95, 
     labels = sprintf("%.0f", CC_OneOff[CC_OneOff$Matrix == matrix[1], AMs_c[4]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(CC_OneOff$Year)/2 + 0.1, 
              max(CC_OneOff$Year) + min(CC_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(CC_OneOffA$Lincomycin)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CC_OneOffA$Lincomycin)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(CC_OneOffA$Lincomycin)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i], ]
  rect(xleft = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Lincomycin,
       col = color_c[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_c)){
  Matrix_data <- CC_OneOff[CC_OneOff$Matrix == matrix_c[i],]
  text(Matrix_data$Year - min(CC_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Lincomycin,
       label = sprintf("%.3f", Matrix_data$Lincomycin),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}
# 5.2.5a. Plot CS
# Set up the plot area and y-axis
plot(unique(CS_OneOff$Year), 
     CS_OneOff[CS_OneOff$Matrix == matrix[1], AMs_c[4]], 
     type = "n",
     xlim = c(min(CS_OneOff$Year)/2 + 0.1, 
              max(CS_OneOff$Year) + min(CS_OneOff$Year)/2 - 0.1),               
     ylim = c(0, unique(CS_OneOff[CS_OneOff$Matrix == matrix[1], AMs_c[4]]*1.2)),     
     xlab = "", ylab = "", 
     main = "Dairy cows with maize silage diet (CS)",
     xaxt = "n", 
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
text(x = par("usr")[1] * 0.4, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentration in manure (mg/kg)", 
     srt = 90, xpd = TRUE, cex = 1.7)
# Add custom x-axis
axis(1, at = unique(CS_OneOff$Year), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add points describing manure concentration
lines(unique(CS_OneOff$Year), 
      CS_OneOff[CS_OneOff$Matrix == matrix[1], AMs_c[4]], 
      type = "o",
      col = "red2", lwd = 3, lty = 6, pch = 7 
)
# Add labels describing C_manure
text(0.8, CS_OneOff[CS_OneOff$Matrix == matrix[1], AMs_c[4]]*0.95, 
     labels = sprintf("%.0f", CS_OneOff[CS_OneOff$Matrix == matrix[1], AMs_c[4]]), 
     pos = 3, cex = 1.3)
# Keep plotting for the primary y-axis before plotting the secondary 
par(new = TRUE)
# Plot the secondary y-axis
plot(0, 0, type = "n", 
     xlim = c(min(CS_OneOff$Year)/2 + 0.1, 
              max(CS_OneOff$Year) + min(CS_OneOff$Year)/2 - 0.1),               # x-axis is the same as that of primary axis
     ylim = c(0, max(CS_OneOffA$Lincomycin)*1.6),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = ""
)
axis(4, at = pretty(c(0, max(CS_OneOffA$Lincomycin)*1.6)), 
     labels = sprintf("%.3f",pretty(c(0, max(CS_OneOffA$Lincomycin)*1.6))), 
     cex.axis = 1.5)
text(x = par("usr")[2] * 1.12, 
     y = mean(par("usr")[3:4]), 
     labels = "Concentrations in other matrix (mg/kg)", 
     srt = 270, xpd = TRUE, cex = 1.7)
for (i in 1:length(comp_c)){
  Matrix_data <- CS_OneOff[CS_OneOff$Matrix == matrix_c[i], ]
  rect(xleft = Matrix_data$Year - min(CS_OneOff$Year)/2 + 0.1 + (i-1)*bar_width, 
       ybottom = 0,
       xright = Matrix_data$Year - min(CS_OneOff$Year)/2 + 0.1 + i*bar_width,
       ytop = Matrix_data$Lincomycin,
       col = color_c[i+1], 
       border = "darkgrey"
  )
}
for(i in 1:length(comp_c)){
  Matrix_data <- CS_OneOff[CS_OneOff$Matrix == matrix_c[i],]
  text(Matrix_data$Year - min(CS_OneOff$Year)/2 + 0.1 + (i-0.5)*bar_width,
       Matrix_data$Lincomycin,
       label = sprintf("%.3f", Matrix_data$Lincomycin),                                                 
       pos = 3,                                                                 # position above the bar
       offset = 0.5,                                                            # adjust the value to move labels up or down
       cex = 1.4,
       col = "black"
  )
}

mtext("Lincomycin (LYN) concentrations in farming compartments and products", outer = TRUE, cex = 2, line = 0.1)
mtext("Year", outer = TRUE, side = 1, cex = 1.7, line = -1)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", 
       legend = comp, col = color_OneOff,
       pch = c(7, rep(22, length(comp)-1)), pt.bg = color_OneOff,               # the background of squares were filed using pt.bg arguments. 22 is for filled squares
       lty = c(6, rep(NA, length(comp)-1)), 
       lwd = c(3, rep(1, length(comp)-1)),
       pt.cex = 2,                                                              # adjust the size of the symbols
       ncol = 3,                                                                # to arrange the legend items in two columns. 
       cex = 1.2,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0.01, 0.01),
       #bty = "n",                                                              # removes the box around the legend for a cleaner look.
       xpd = TRUE,                                                              # this allows plotting outside the figure region
       x.intersp = 0.2,
       y.intersp = 0.7,
       text.width = 0.4
)
min(summary(Result_SA_milk))
min(Result_SA_milk$`Sensitivity range`)
max(Result_SA_milk$`Sensitivity range`)


# 5.3 Sensitivity analysis plotting
# Determination of layout parameters
Nr_SAVariables <- dim(Result_SA)[2] - 2
color_SA <- c("firebrick3","lightsalmon4",
              "darkgoldenrod2", "mediumpurple", "steelblue4", "grey28", 
              "darkgreen","darkolivegreen3")
pch_SA <- c(1:8)
par(mfrow = c(2,1), oma = c(3, 5, 7, 2), mar = c(4, 6, 4, 4), las = 1)
# Set up basic plot for SA of C_milk
plot(0, 
     0,
     type = "n",
     xlim = c(min(Result_SA_milk$`Sensitivity range`) - 0.02, 
              max(Result_SA_milk$`Sensitivity range`) + 0.02),               
     ylim = c(min(Result_SA_milk$`Sensitivity range`) - 0.02, 
              max(Result_SA_milk$`Sensitivity range`) + 0.02),     
     xlab = "", ylab = "", 
     main = "Sensitivity analysis for AM concentration in milk",
     xaxt = "n", 
     yaxt = "n",
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
# Add x-axis title
text(x = par("usr")[1]*1.18, 
     y = mean(par("usr")[3:4]), 
     labels = "Variation of AM concentration in milk (%)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add custom x-axis
axis(1, at = c(seq(-0.5,0.5, by = 0.25)), labels = c(seq(-50,50, by = 25)), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add y-axis title
text(x = mean(par("usr")[2:3]), 
     y = par("usr")[1]*1.3, 
     labels = "Variation of input parameters (%)", 
     srt = 0, xpd = TRUE, cex = 1.5)
# Add custom y-axis
axis(2, at = c(seq(-0.5,0.5, by = 0.25)), labels = c(seq(-50,50, by = 25)), cex.axis = 1.5, mgp = c(3,1.5,0))
# Plotting C_milk
for (i in 1:Nr_SAVariables){
  SA_data <- Result_SA_milk[,i+2]
  lines(Result_SA_milk$`Sensitivity range`, 
       SA_data,
       type = "b",
       col = color_SA[i], 
       xaxt = "n",
       yaxt = "n",
       pch = i
  )
}
# Add plot marks
text(x = -0.5,
     y = 0.5,
     labels = "A",
     srt = 0,
     cex = 2)
# Set up basic plot for SA of Q_admin
plot(0, 
     0,
     type = "n",
     xlim = c(min(Result_SA_Admin$`Sensitivity range`) - 0.02, 
              max(Result_SA_Admin$`Sensitivity range`) + 0.02),               
     ylim = c(min(Result_SA_Admin$`Sensitivity range`) - 0.3, 
              max(Result_SA_Admin$`Sensitivity range`) + 0.3),     
     xlab = "", ylab = "", 
     main = "Sensitivity analysis for AM quantity consumed from feed",
     xaxt = "n", 
     yaxt = "n",
     cex.axis = 1.5, cex.main = 1.5,
     mgp = c(3, 1, 0)
)
# Add y-axis title
text(x = par("usr")[1]*1.18, 
     y = mean(par("usr")[3:4]), 
     labels = "Variation of AM quantity consumed (%)", 
     srt = 90, xpd = TRUE, cex = 1.5)
# Add x-axis title
text(x = 0, 
     y = par("usr")[1]*2, 
     labels = "Variation of input parameters (%)", 
     srt = 0, xpd = TRUE, cex = 1.5)

# Add custom x-axis
axis(1, at = c(seq(-0.5,0.5, by = 0.25)), labels = c(seq(-50,50, by = 25)), cex.axis = 1.5, mgp = c(3,1.5,0))
# Add custom y-axis
axis(2, at = c(seq(-0.8,0.8, by = 0.4)), labels = c(seq(-80,80, by = 40)), cex.axis = 1.5, mgp = c(3,1.5,0))
# Plotting Q_admin
for (i in 1:Nr_SAVariables){
  SA_data <- Result_SA_Admin[,i+2]
  lines(Result_SA_Admin$`Sensitivity range`, 
        SA_data,
        type = "b",
        col = color_SA[i], 
        xaxt = "n",
        yaxt = "n",
        pch = i
  )
}
# Add plot marks
text(x = -0.5,
     y = 0.77,
     labels = "B",
     srt = 0,
     cex = 2)
# Create a global legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", legend = colnames(Result_SA_milk)[3:(Nr_SAVariables+2)], 
       ncol = 4,                                                                # to arrange the legend items in two columns. 
       cex = 1.2,                                                               # the cex parameter adjusts the overall size of the legend. You may need to tweak this.
       inset = c(0, 0),
       col = color_SA, 
       pch = pch_SA,
       lty = 1,
       x.intersp = 0.2,
       y.intersp = 0.7,
       text.width = 0.2
    )

