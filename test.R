library(rgl)
library(webshot2)
library(plotly)
options(rgl.printRglwidget = FALSE)

M <- c(2,2.4,2.5,2.8,3,3.4,3.6,3.8,4,4.4,4.6,5)
Mach_No <- rep(M, each=7)                        #Repeat each M for no. of AOAs

AOA <- c(rep(c(0,2.5,5,7.5,10,12.5,15),12))      #Repeat the set of AOAs for no. of Mach nos

Cd <- c(0.01736,0.017414,0.017691,0.018109,0.018754,0.01972,0.02132,
        0.013798,0.013849,0.014088,0.014453,0.014963,0.015633,0.016467,
        0.013154,0.013198,0.013445,0.013809,0.014335,0.014987,0.015805,
        0.01154,0.011596,0.011852,0.012251,0.0128,0.013489,0.0143,
        0.010686,0.010746,0.011016,0.011437,0.012022,0.012745,0.013583,
        0.00933,0.009398,0.009699,0.010178,0.010837,0.011524,0.012531,
        0.008682,0.008753,0.009066,0.00957,0.010258,0.011089,0.012004,
        0.008204,0.00828,0.00861,0.009143,0.009867,0.010722,0.011681,
        0.00778,0.00786,0.008207,0.00877,0.009528,0.010417,0.011413,
        0.007059,0.007149,0.007524,0.00815,0.008965,0.009927,0.010993,
        0.00675,0.006845,0.007237,0.00789,0.008734,0.009735,0.010827,
        0.006212,0.006318,0.006743,0.007448,0.008349,0.009407,0.010563)

dataF <- data.frame(Mach_No,AOA,Cd)
print(dataF)

model <- lm(Cd ~ poly(Mach_No, 3,raw = TRUE) + poly(AOA, 3, raw = TRUE), data = dataF)

# Generating a grid of values for the predictors
Mach_Seq <- seq(min(dataF$Mach_No), max(dataF$Mach_No), length.out = 100)
Angle_Seq <- seq(min(dataF$AOA), max(dataF$AOA), length.out = 100)
grid <- expand.grid(Mach_No = Mach_Seq, AOA = Angle_Seq)

# Making predictions on the grid
grid$Cd <- predict(model, newdata = grid)


open3d()
bg3d("#E2FEFF")
plot3d(dataF$Mach_No, dataF$AOA, dataF$Cd,
       xlab = "Mach No", ylab = "AOA", zlab = "Cd",
       col = "blue", size = 5,axes = TRUE)
surface3d(unique(grid$Mach_No), unique(grid$AOA), matrix(grid$Cd, nrow = 100),
          col = "#000000", alpha = 0.8)
axes3d()

summary(model)
print(model$coefficients)