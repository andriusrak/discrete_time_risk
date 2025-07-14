#turto ir isipareigojimu istoriniai duomenys. naujesni duomenys pirmi - ivedimo tvarka: nuo naujausio iki seniausio
turtas <- c(11250, 11000, 10400, 10500, 10000)
  #c(641370, 623355, 617415, 606828, 604904, 584702, 609824, 602351, 604125, 594729)
  #c(8000,7000,6000,5000)
isipareigojimai <- c(10000, 9500, 9250, 9100, 9000)
  #c(611624, 590919, 585961, 576472, 576010, 553477, 579454, 573266, 575944, 563885)
  #c(6000,5000,4000,3000)

#kapitalas = turtas - isipareigojimai
u <- turtas - isipareigojimai 

#A0 <- turtas[1]
#B0 <- isipareigojimai[1]
u0 <- u[1]
#apskaiciuojamas turto ir isipareigojimu kitimas tarp ketvirciu
turto_kitimas <- -diff(turtas)
isip_kitimas <- -diff(isipareigojimai)

rizikos_par = 2 #parametras pagal kuri sudaromas zalu skirstinys pagal formule vidurkis + [parametras] * standartinis_nuokrypis

# funkcija sudaryti turto ir isipareigojimu skirstini paremnta normalia distribucija
pmf_skirst <- function(values, n = 31) {
  sd_val <- sd(values) #standartinis nuokrypis
  mean_val <- mean(values) #vidurkis
  x_vals <- seq(mean_val - rizikos_par * sd_val, mean_val + rizikos_par * sd_val, length.out = n) #sudaroma seka naudojant vidurki ir std nuokrypi
  x_vals <- round(x_vals)
  pdf_vals <- dnorm(x_vals, mean = mean_val, sd = sd_val) #naudojama R pdf(probability density function) sudarymo funkcija
  pmf_vals <- pdf_vals / sum(pdf_vals) #tikimybes sulyginamos, kad suma =1 
  list(x = x_vals, pmf = pmf_vals)
}

turto <- pmf_skirst(turto_kitimas)
isip <- pmf_skirst(isip_kitimas)
turto
isip
# sudaromas kapitalo kitimo skirstinys
n_turto <- length(turto$x)
n_isip <- length(isip$x)

deltaU <- outer(turto$x, isip$x, "-") #kapitalo kitimo matrica
p_matrica <- outer(turto$pmf, isip$pmf) #tikimybiu matrica

deltaU_reiksm <- as.vector(deltaU)
p_matrica <- as.vector(p_matrica)

#skirstinio grafikas
plot(deltaU_reiksm, p_matrica, type= "l")

# agreguoja vektorius pasalina duplikatus
df <- aggregate(p_matrica, by = list(deltaU_reiksm), FUN = sum)
deltaU_reiksm <- df$Group.1
p_matrica <- df$x

expected_value <- sum(deltaU_reiksm * p_matrica)
cat("Skirstinio EV (kapitalo kitimo vidurkis):", expected_value, "\n")

# laiko ir kapitalo ribos parametrai
laikas <- 12
lubos <- u0 * 5 #kad papildomai neapkrauti programos nustatom riba virs kurios isgyvenimo tikimybe ~1

# sudaroma matrica laikyti busenas
phi <- matrix(0, nrow = laikas + 1, ncol = lubos + 1) #eilutes - laikas, stulpeliai kapitalas
phi[1, u0 + 1] <- 1  #isgyvenimo tikimybe  phi(u0,t=0) = 1


for (t in 1:laikas) {

  cat("Iteracija t=", t, "\n")
  isgyvenimo_busenos <- which(phi[t, ] > 0) - 1
  
  for (u in isgyvenimo_busenos) {
    
    probs <- phi[t, u + 1] * p_matrica #apskaiciuojama kiekvienos galimos busenos tikimybe
    new_u <- pmin(u + deltaU_reiksm, lubos) #paimamos reiksmes nevirsijancios lubu ribos
    gyvas <- which(new_u >= 0) #busenos kuriu kapitalas > 0 

    idx <- new_u[gyvas] + 1
    phi[t + 1, idx] <- phi[t + 1, idx] + probs[gyvas]
  }
}

bankroto_tikimybes_per_t <- 1 - rowSums(phi)

# Bankroto tikimybes per laika t lentele
bankroto_tikimybe <- 1 - sum(phi[laikas + 1, ])
cat("Bankroto tikimybe per", laikas, "laiko momentu:", bankroto_tikimybe, "\n")


# rezultatai
bankroto_lentele <- data.frame(Laikas = 0:laikas, Bankroto_tikimybe = bankroto_tikimybes_per_t)
print(bankroto_lentele)