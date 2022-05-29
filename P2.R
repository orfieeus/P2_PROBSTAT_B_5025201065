#No 1
install.packages("BSDA")

library(BSDA)

orang.ke = c(seq(1:9))
oksigen.sebelum = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
oksigen.sesudah = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
data = data.frame(orang.ke,oksigen.sebelum, oksigen.sesudah)
n = 9

#A
selisih = data$oksigen.sesudah - data$oksigen.sebelum
cat("Standar deviasi selisih pasangan pengamatan : ")
standardev = sd(selisih)
standardev

#B
mu = 0
xbar = mean(selisih)
tstastistik =( (xbar - mu) / (standardev / sqrt(n)))
pvalue = 2 * pt(-abs(tstastistik), df=n-1)
pvalue


#C

t.test(x=data$oksigen.sesudah, y=data$oksigen.sebelum,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)


#No 2
standardev = 3900
n = 100
xbar = 23500
mu = 20000
zstastistik =( (xbar - mu) / (standardev / sqrt(n)))
pvalue = pnorm(-abs(zstastistik))
pvalue

#No 3
significantlevel = 0.05
nBandung = 19
nBali = 27
meanBandung = 3.64
meanBali = 2.79
sdBandung = 1.67
sdBali = 1.32

SP2 = ((nBandung-1)*sdBandung*sdBandung + (nBali-1)*sdBali*sdBali)/ (nBandung+nBali-2)
tstatistik = (meanBandung-meanBali - 0)/(sqrt(SP2*(1/nBandung + 1/nBali)))

t = qt(p=significantlevel, df=2, lower.tail=TRUE)
batasAtas = (meanBandung - meanBali) - t*(sqrt(SP2*(1/nBandung + 1/nBali)))
batasBawah = (meanBandung - meanBali) + t*(sqrt(SP2*(1/nBandung + 1/nBali)))
batasAtas
batasBawah