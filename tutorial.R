install.packages("readxl") 
install.packages("meta") 
install.packages("metafor")
install.packages("metasens")
# Above 4 lines are only required when running the first time

# Once the packages are installed, we can run just the next line onwards 
# whenever we perform a meta-analysis.
library("readxl")
library("meta")
library("metafor")
library("metasens")


# Set working directory (WD)
  # Press Ctrl + Shift + H
  # Alternatively, you can go to Session -> Set Working Directory -> Choose Directory
  # Here, choose a folder where you wish to store all the files
  # This includes the excel file that contains the extracted data
  # This will also include the output files (plots or images)

getwd() #This shows the current WD. So, you can verify whether it has been set or not.

#importing the excel file, and inspecting it
data <- read_xlsx("tutorial_md.xlsx")
#the next two lines are not absolutely necessary. But, these help to 
#view the imported data and see if everything looks fine
View(data) #you can inspect the imported data
str(data)

# making the meta-analysis object named metta
metta <- metacont(n.e = n.e ,
                  mean.e = mean.e, 
                  sd.e = sd.e,
                  n.c = n.c, 
                  mean.c = mean.c, 
                  sd.c = sd.c, 
                  studlab = authors, 
                  data = data, 
                  sm = "MD", 
                  prediction = TRUE)

# viewing the meta-analysis
metta
summary(metta)

# basic forest plot
forest.meta(metta)

# customised forest plot
forest.meta(x = metta, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c", 
            pooled.events = T, 
            text.addline1 = "Inverse variance method", 
            text.addline2 = "Restricted maximum likelihood-estimator for tau^2, Q-Profile method for CI of tau^2", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab
            )

# Subgroup analysis
metta.sg <- update.meta(object = metta, 
                        subgroup = Age, 
                        print.subgroup.name = FALSE)
metta.sg

# Subgroup analysis: forest plot
forest.meta(x = metta.sg)

# Meta-regression
  #Translating into metafor language, and then adding LDL as a moderator
metta.mr <- rma(yi = metta$TE, sei = metta$seTE, 
                method = metta$method.tau, mods = data$LDL)
summary(metta.mr)

# Meta-regression: basic bubble plot
regplot(metta.mr)

# Meta-regression: customised detailed bubble plot
regplot(x = metta.mr, pi = T, legend = TRUE, 
        xlab = "LDL", ylab = "Mean difference in serum HbA1c", 
        col = "brown", lcol = c("red","green","blue","black"), label ="piout",
        main = "Meta-regression: Bubble plot with 
        confidence and prediction intervals")
subtitle <- paste0("Test of Moderators: ",
                   "Beta = ", round(metta.mr$beta[2], digits = 2), 
                   " (", round(metta.mr$ci.lb[2], digits = 2),
                   " - ", round(metta.mr$ci.ub[2], digits = 2), ")",
                   ", p = ", round(metta.mr$pval[2], digits = 2)
)
mtext(subtitle, side = 1, line = 4, font = 2)

# Publication bias: basic funnel plot
funnel.meta(metta)

# Publication bias: basic funnel plot with study labelling
funnel.meta(metta, studlab = TRUE)

# Publication bias: Customised funnel plot
  #First, we are updating our meta-analysis object by 
  #including trim and fill calcuations
meta.tf <- trimfill(x = metta)
meta.tf
  #Next, we start building the customised funnel plot
contour <- c(0.9, 0.95, 0.99)
col_contour <- c("gray50", "gray75", "gray95")
col_contour2 <- c("gray50", "gray75", "gray95", "#003366")
legend_funnel <- c("p < 0.1", "p < 0.05", "p < 0.01", "Summary estimate")
egger <- metabias(x = metta, method.bias = "Egger")
egger.report <- paste0("Egger's regression: ", round(egger$estimate[1],2), 
                      " (", round(egger$estimate[1] - 1.959964*egger$estimate[2],2), 
                      " - ", round(egger$estimate[1] + 1.959964*egger$estimate[2],2), ")",
                      ", p = ", round(egger$pval, 2), 
                      sep = "")
funnel.meta(x = meta.tf, 
            contour.levels = contour, 
            col.contour = col_contour, common = F, random = T, xlab = "Estimate",
            col = "blue", bg = "blue" ,
            col.random = "#003366",col.ref = "black", xlim = c(-0.9,1)
            )
legend(x = 0.2,y = 0,legend = legend_funnel, fill = col_contour2)
title("Contour-enhanced Trim-and-Fill Funnel Plot showing imputed studies")
mtext(egger.report, side = 1, line = 4, font = 2)


# Publication bias: Egger's regression
metabias(x = metta, method.bias = "Egger")

# Publication bias: Egger's regression with plot
metabias(x = metta, method.bias = "Egger", plotit = TRUE)


# Publication bias: Doi plot and LFK index
doiplot(metta)

# Publication bias: Reformatted Doi plot with LFK index
# Also, explanation on exporting an image
pdf(file = "doi.pdf", height = 10, width = 5)
doiplot(metta, xlab = "Transformed estimates", 
        main = "Effect of Drug X v/s placebo on HbA1c", 
        pos.lfkindex = "topright")
dev.off()
# Enclosing a plot between a pdf(....) command in the previous line 
# and dev.off() in the next line exports the plot at the working directory 
# with the name as supplied in the pdf argument

#Sensitivity analysis for study quality
metta.rob <- update.meta(object = metta, subgroup = quality )
metta.rob

#If you wish to rerun a forest plot with only low quality studies
  #You can redefine the dataframe to only include high quality studies
data.highquality <- data[data$quality == "high", ]
  #And then, you can rerun the meta-analysis with this data.high quality 
  #instead of data. And similarly, you can proceed for a forest plot using 
  #this new meta-analysis object


#Sensitivity analysis for leave-one-out
metta.l1o <- metainf(metta, pooled = "random")
forest.meta(metta.l1o)


#Drapery plot
drapery(metta, legend = FALSE, main = "Effect against a range of p values")



# OTHER TYPES OF META-ANALYSIS
# Two group - proportion meta-analysis
data_2_prop <- read_xlsx("tutorial_rr.xlsx")
metta.bin <- metabin(event.e = event.e, 
                     n.e = n.e,
                     event.c = event.c,
                     n.c = n.c, 
                     studlab = authors, 
                     data = data_2_prop, 
                     sm = "RR", 
                     prediction = TRUE)
# Here, this performs a meta-analysis of risk ratios
# Just change the sm argument, and you can perform other 2-group proportional meta-analyses 
# sm = "OR" -> odds ratio meta-analysis
# sm = "RD" -> risk difference meta-analysis
# See help(metabin) for more information

# Now we have a meta-analysis object named metta.bin (like metta earlier)
# Now, we can run the analyses like earlier just by changing the meta-analysis 
# object supplied to the functions 
summary(metta.bin)
# Similarly, you can try with the other analyses


# One group - meta-analysis of mean and SD
  # We can try this with the earlier datasheet that we used for mean difference meta-analysis
  # We will just take data for the experimental group, ignoring control
metta.mean <- metamean(n = n.e ,
                  mean = mean.e, 
                  sd = sd.e,
                  studlab = authors, 
                  data = data, 
                  prediction = TRUE)
# Next, you can proceed as earlier
# For more info, you may check help(metamean)


# One group - meta-analysis of proportion
# We can try this with the earlier datasheet that we used for risk ratio meta-analysis
# We will just take data for the experimental group, ignoring control
data_2_prop <- read_xlsx("tutorial_rr.xlsx")

metta.prop <- metaprop(event = event.e, 
                     n = n.e,
                     studlab = authors, 
                     data = data_2_prop, 
                     prediction = TRUE)
# Next, you can proceed as earlier
# For more info, you may check help(metaprop)

# Some of the customised plots shown in the main article in the journal:

# customised forest plot


pdf("figure_6.pdf", width = 12, height = 5)
forest.meta(x = metta, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c", 
            pooled.events = T, 
            text.addline1 = "Inverse variance method", 
            text.addline2 = "Restricted maximum likelihood-estimator for tau^2, Q-Profile method for CI of tau^2", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab
)
dev.off()

# Subgroup analysis: forest plot
pdf("figure_7.pdf", width = 12)
forest.meta(x = metta.sg, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c",
            pooled.events = T, 
            text.addline1 = "Subgroup Analysis", 
            text.addline2 = "Inverse variance method", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab 
)
dev.off()

# Meta-regression: customised detailed bubble plot
pdf("figure_8.pdf")
regplot(x = metta.mr, pi = T, legend = TRUE, 
        xlab = "LDL", ylab = "Mean difference in serum HbA1c", 
        col = "brown", lcol = c("red","green","blue","black"), label ="piout",
        main = "Meta-regression: Bubble plot with confidence and prediction intervals")
subtitle <- paste0("Test of Moderators: ",
            "Beta = ", round(metta.mr$beta[2], digits = 2), 
            " (", round(metta.mr$ci.lb[2], digits = 2),
            " - ", round(metta.mr$ci.ub[2], digits = 2), ")",
            ", p = ", round(metta.mr$pval[2], digits = 2)
            )
mtext(subtitle, side = 1, line = 4, font = 2)
dev.off()

# Funnel plot
pdf("figure_9.pdf")
meta.tf <- trimfill(x = metta)
meta.tf
contour <- c(0.9, 0.95, 0.99)
col_contour <- c("gray50", "gray75", "gray95")
col_contour2 <- c("gray50", "gray75", "gray95", "#003366")
legend_funnel <- c("p < 0.1", "p < 0.05", "p < 0.01", "Summary estimate")
egger <- metabias(x = metta, method.bias = "Egger")
egger.report <- paste0("Egger's regression: ", round(egger$estimate[1],2), 
                       " (", round(egger$estimate[1] - 1.959964*egger$estimate[2],2), 
                       " - ", round(egger$estimate[1] + 1.959964*egger$estimate[2],2), ")",
                       ", p = ", round(egger$pval, 2), 
                       sep = "")
funnel.meta(x = meta.tf, 
            contour.levels = contour, 
            col.contour = col_contour, common = F, random = T, xlab = "Estimate",
            col = "blue", bg = "blue" ,col.random = "#003366",col.ref = "black", xlim = c(-0.9,1)
)
legend(x = 0.2,y = 0,legend = legend_funnel, fill = col_contour2)
title("Contour-enhanced Trim-and-Fill Funnel Plot showing imputed studies")
mtext(egger.report, side = 1, line = 4, font = 2)
dev.off()

#Doi plot
pdf(file = "figure_10.pdf", height = 10, width = 5)
doiplot(metta, xlab = "Transformed estimates", 
        main = "Effect of Drug X v/s placebo on HbA1c", 
        pos.lfkindex = "topright")
dev.off()

#Sensitivity analysis for leave-one-out
pdf("figure_11.pdf", width = 9, height = 5)
forest.meta(x = metta.l1o, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c",
            pooled.events = T, 
            text.addline1 = "Leave-one-out meta-analysis", 
            text.addline2 = "Sensitivity analysis by omitting each study one by one", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab 
)
dev.off()

#Drapery plot
pdf("figure_12.pdf", width = 9)
drapery(metta, legend = FALSE, main = "Effect against a range of p values", 
        labels = FALSE)
dev.off()


# PNG images


png("figure_6.png", width = 3600, height = 1500, res = 300)
forest.meta(x = metta, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c", 
            pooled.events = T, 
            text.addline1 = "Inverse variance method", 
            text.addline2 = "Restricted maximum likelihood-estimator for tau^2, Q-Profile method for CI of tau^2", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab
)
dev.off()

# Subgroup analysis: forest plot
png("figure_7.png", width = 3600, height = 2100, res = 300)
forest.meta(x = metta.sg, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c",
            pooled.events = T, 
            text.addline1 = "Subgroup Analysis", 
            text.addline2 = "Inverse variance method", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab 
)
dev.off()

# Meta-regression: customised detailed bubble plot
png("figure_8.png", width = 2100, height = 2100, res = 300)
regplot(x = metta.mr, pi = T, legend = TRUE, 
        xlab = "LDL", ylab = "Mean difference in serum HbA1c", 
        col = "brown", lcol = c("red","green","blue","black"), label ="piout",
        main = "Meta-regression: Bubble plot with confidence and prediction intervals")
subtitle <- paste0("Test of Moderators: ",
                   "Beta = ", round(metta.mr$beta[2], digits = 2), 
                   " (", round(metta.mr$ci.lb[2], digits = 2),
                   " - ", round(metta.mr$ci.ub[2], digits = 2), ")",
                   ", p = ", round(metta.mr$pval[2], digits = 2)
)
mtext(subtitle, side = 1, line = 4, font = 2)
dev.off()

# Funnel plot
png("figure_9.png", width = 2100, height = 2100, res = 300)
meta.tf <- trimfill(x = metta)
meta.tf
contour <- c(0.9, 0.95, 0.99)
col_contour <- c("gray50", "gray75", "gray95")
col_contour2 <- c("gray50", "gray75", "gray95", "#003366")
legend_funnel <- c("p < 0.1", "p < 0.05", "p < 0.01", "Summary estimate")
egger <- metabias(x = metta, method.bias = "Egger")
egger.report <- paste0("Egger's regression: ", round(egger$estimate[1],2), 
                       " (", round(egger$estimate[1] - 1.959964*egger$estimate[2],2), 
                       " - ", round(egger$estimate[1] + 1.959964*egger$estimate[2],2), ")",
                       ", p = ", round(egger$pval, 2), 
                       sep = "")
funnel.meta(x = meta.tf, 
            contour.levels = contour, 
            col.contour = col_contour, common = F, random = T, xlab = "Estimate",
            col = "blue", bg = "blue" ,col.random = "#003366",col.ref = "black", xlim = c(-0.9,1)
)
legend(x = 0.2,y = 0,legend = legend_funnel, fill = col_contour2)
title("Contour-enhanced Trim-and-Fill Funnel Plot showing imputed studies")
mtext(egger.report, side = 1, line = 4, font = 2)
dev.off()

#Doi plot
png(file = "figure_10.png", width = 1500, height = 3000, res = 300)
doiplot(metta, xlab = "Transformed estimates", 
        main = "Effect of Drug X v/s placebo on HbA1c", 
        pos.lfkindex = "topright")
dev.off()

#Sensitivity analysis for leave-one-out
png("figure_11.png", width = 2700, height = 1500, res = 300)
forest.meta(x = metta.l1o, 
            random = T, 
            common = F, 
            layout = "RevMan5", 
            col.diamond.random = "Green", 
            col.random = "Green",
            col.square = "Blue", 
            col.square.lines = "Blue",
            col.study = "Blue",
            text.random = "Mean difference (random effects model)",
            digits = 2, digits.se = 2,
            smlab = "Serum HbA1c",
            pooled.events = T, 
            text.addline1 = "Leave-one-out meta-analysis", 
            text.addline2 = "Sensitivity analysis by omitting each study one by one", 
            header.line = "both", 
            label.e = "Drug X", 
            label.c = "Control", 
            label.left = "Lower in Drug X", 
            label.right = "Higher in Drug X", 
            sortvar = studlab 
)
dev.off()

#Drapery plot
png("figure_12.png", width = 2700, height = 2100, res = 300)
drapery(metta, legend = FALSE, main = "Effect against a range of p values", 
        labels = FALSE)
dev.off()


# Continue experimenting with different real datasets
# And see the package documentation and 
# And read articles including 
      # our planned series of articles for greater insight
# ALL THE BEST!
# - The team behind "How to perform meta-analysis in R? A simple yet comprehensive guide" 
# at the journal "The Evidence"