# Julio Huato
# El desempeño económico de Mexico en la posguerra
# Grupo de Chapingo
# 19 de septiembre de 2016

cat("\014") # Limpia la consola
pwt9 <- read.csv("~/Downloads/pwt9.csv") # Carga los datos (PWT 9.0)
str(pwt9) # Estructura de los datos
pwt <- pwt9[c(-2,-3,-(11:20),-22,-23,-(25:35), -(39:47))] # Datos reducidos
str(pwt) # Estructura de los datos reducidos
summary(pwt) # Resumen estadístico de los datos reducidos
pwt$Y <- (pwt$rgdpe+pwt$rgdpo)/2 # PIB real (promedio de 2 métodos)
pwt$N <- pwt$pop # N: población
pwt$E <- pwt$emp # E: empleo
pwt$L <- pwt$E*pwt$avh # L: horas de trabajo
pwt$H <- pwt$L*pwt$hc # H: horas de trabajo (ajuste por capital humano H)
pwt$Ypc <- (pwt$Y/pwt$pop) # Ypc: PIB/habitante
pwt$Ypw <- (pwt$Y/pwt$E) # Ypw: PIB/trabajador
pwt$Yph <- (pwt$Y/pwt$L) # Yph: PIB/hora de trabajo
pwt$Yphh <- (pwt$Y/pwt$H) # Yphh: PIB/hora de trabajo (ajuste H)
pwt$W <- (pwt$labsh*pwt$Y) # W: salarios
pwt$P <- (pwt$Y - pwt$W) # P: gananancias brutas
pwt$e <- (pwt$P/pwt$W) # e: tasa de explotación
pwt$rho <- (pwt$Y/pwt$rkna) # rho: productividad del capital (tasa máxima de ganancia)
pwt$r <- (pwt$P/pwt$rkna) # r: tasa de ganancia
pwt$c <- (pwt$csh_c+pwt$csh_g) # c: cuota de consumo (C/Y)
pwt$C <- ((pwt$csh_c+pwt$csh_g)*pwt$Y) # C: gastos de consumo (privado y público)
pwt$I <- (pwt$csh_i*pwt$Y) # I: gasto de inversión (privada y pública)
pwt$e1 <- (pwt$I/pwt$C) # e1: tasa de inversión-consumo (privados y públicos)
pwt$r1 <- (pwt$I/pwt$rkna) # r1: tasa de acumulación (privada y pública)
str(pwt) # Estructura de los nuevos datos
mex <- pwt[ which(pwt$countrycode=="MEX"),] # Datos de México
str(mex) # Estructura de los datos de México
summary(mex) # Resumen estadístico de los datos de México
usa <- pwt[ which(pwt$countrycode=="USA"),] # Datos de EEUU
str(usa) # Estructura de los datos de EEUU
summary(usa) # Resumen estadístico de los datos de China
chn <- pwt[ which(pwt$countrycode=="CHN"),] # Datos de China
str(chn) # Estructura de los datos de China
summary(chn) # Resumen estadístico de los datos de China
bra <- pwt[ which(pwt$countrycode=="BRA"),] # Datos de Brasil
str(bra) # Estructura de los datos de Brasil
summary(bra) # Resumen estadístico de los datos de Brasil

# Gráficas 
pdf("mexYpc.pdf")
plot(c(1950:2014), mex$Ypc, type="l", lwd=3, col="dark green", main="México: PIB/habitante (1950-2014)", xlab = "Año", ylab="USD 2011", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexYpw.pdf")
plot(c(1950:2014), mex$Ypw, type="l", lwd=3, col="dark green", main="México: PIB/trabajador (1950-2014)", xlab = "Año", ylab="USD 2011 ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexYph.pdf")
plot(c(1950:2014), mex$Yph, type="l", lwd=3, col="dark green", main="México: PIB/hora (1950-2014)", xlab = "Año", ylab="USD 2011", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexYphh.pdf")
plot(c(1950:2014), mex$Yphh, type="l", lwd=3, col="dark green", main="México: PIB/hora (ajuste-H) (1950-2014)", xlab = "Año", ylab="USD 2011", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexlabsh.pdf")
plot(c(1993:2014), mex$labsh[44:65], type="l", lwd=3, col="dark green", main="México: Salarios/PIB (1993-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexe.pdf")
plot(c(1993:2014), mex$e[44:65], type="l", lwd=3, col="dark green", main="México: Ganancia bruta/salarios (1993-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexc.pdf")
plot(c(1950:2014), mex$c, type="l", lwd=3, col="dark green", main="México: Consumo/PIB (1950-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexe1.pdf")
plot(c(1950:2014), mex$e1, type="l", lwd=3, col="dark green", main="México: Inversión/consumo (1950-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexrho.pdf")
plot(c(1950:2014), mex$rho, type="l", lwd=3, col="dark green", main="México: PIB/capital (1950-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexr.pdf")
plot(c(1950:2014), mex$r, type="l", lwd=3, col="dark green", main="México: Tasa de ganancia (1950-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

pdf("mexr1.pdf")
plot(c(1950:2014), mex$r1, type="l", lwd=3, col="dark green", main="México: Inversión/capital (1950-2014)", xlab = "Año", ylab=" ", sub="Fuente: PWT 9.0")
dev.off()

# La brecha ante el mundo
pdf("Ypc.pdf")
plot(c(1950:2014), usa$Ypc, type="l", col="blue", lwd=3, main="PIB/habitante (1950-2014)", xlab = "Año", ylab="USD 2011", ylim=c(0, 55000), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$Ypc, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$Ypc, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$Ypc, type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("Ypw.pdf")
plot(c(1950:2014), usa$Ypw, type="l", col="blue", lwd=3,  main="PIB/trabajador (1950-2014)", xlab = "Año", ylab="USD 2011", ylim=c(0, 110000), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$Ypw, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$Ypw, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$Ypw, type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("Yph.pdf")
plot(c(1950:2014), usa$Yph, type="l", col="blue", lwd=3, main="PIB/hora (1950-2014)", xlab = "Año", ylab="USD 2011", ylim=c(0, 65), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$Yph, type="l", lwd=3, col="dark green")
lines(c(1950:2014), bra$Yph, type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "Brasil"), lwd=c(2,2,2), lty=c(1,1,1), col=c("dark green","blue","yellow"))
dev.off()

pdf("Yphh.pdf")
plot(c(1950:2014), usa$Yphh, type="l", col="blue", lwd=3, main="PIB/hora (ajuste H) (1950-2014)", xlab = "Año", ylab="USD 2011", ylim=c(0, 18), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$Yphh, type="l", lwd=3, col="dark green")
lines(c(1950:2014), bra$Yphh, type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "Brasil"), lwd=c(2,2,2), lty=c(1,1,1), col=c("dark green","blue","yellow"))
dev.off()

pdf("labsh.pdf")
plot(c(1993:2014), usa$labsh[44:65], type="l", col="blue", lwd=3,  main="Cuota salarial (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0.3, 0.7), sub="Fuente: PWT 9.0")
lines(c(1993:2014), mex$labsh[44:65], type="l", lwd=3, col="dark green")
lines(c(1993:2014), chn$labsh[44:65], type="l", lwd=3, col="red")
lines(c(1993:2014), bra$labsh[44:65], type="l", lwd=3, col="yellow")
legend("bottomleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("c.pdf")
plot(c(1950:2014), usa$c, type="l", col="blue", lwd=3,  main="Cuota de consumo (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0.45, .95), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$c, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$c, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$c, type="l", lwd=3, col="yellow")
legend("bottomleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()


pdf("e.pdf")
plot(c(1993:2014), usa$e[44:65], type="l", col="blue", lwd=3,  main="Tasa de explotación (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0.3, 1.6), sub="Fuente: PWT 9.0")
lines(c(1993:2014), mex$e[44:65], type="l", lwd=3, col="dark green")
lines(c(1993:2014), chn$e[44:65], type="l", lwd=3, col="red")
lines(c(1993:2014), bra$e[44:65], type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("rho.pdf")
plot(c(1950:2014), usa$rho, type="l", col="blue", lwd=3, main="Productividad del capital (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0, 1.1), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$rho, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$rho, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$rho, type="l", lwd=3, col="yellow")
legend("topright", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("r.pdf")
plot(c(1950:2014), usa$r, type="l", col="blue", lwd=3, main="Tasa de ganancia (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0, .4), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$r, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$r, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$r, type="l", lwd=3, col="yellow")
legend("topright", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("e1.pdf")
plot(c(1950:2014), usa$e1, type="l", col="blue", lwd=3, main="Tasa de inversión-consumo (1950-2014)", xlab = "Año", ylab=" ", ylim=c(0, 1), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$e1, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$e1, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$e1, type="l", lwd=3, col="yellow")
legend("topleft", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()

pdf("r1.pdf")
plot(c(1950:2014), usa$r1, type="l", col="blue", lwd=3, main="Tasa de acumulación (1950-2014)", xlab = "Año", ylab="Inversión (pública y privada)/capital", ylim=c(0, .22), sub="Fuente: PWT 9.0")
lines(c(1950:2014), mex$r1, type="l", lwd=3, col="dark green")
lines(c(1950:2014), chn$r1, type="l", lwd=3, col="red")
lines(c(1950:2014), bra$r1, type="l", lwd=3, col="yellow")
legend("topright", c("Mexico","EEUU", "China", "Brasil"), lwd=c(3,3,3,3), lty=c(1,1,1,1), col=c("dark green","blue","red","yellow"))
dev.off()
