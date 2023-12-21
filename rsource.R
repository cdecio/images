base=read.csv("gpcm.csv", header = T)
#COGFLEX####
COGFLEX=base[,4:6]
model.gpcm.COGFLEX <- 'COGFLEX= 1-3' 
results.gpcm.COGFLEX <- mirt(data=COGFLEX, model=model.gpcm.COGFLEX, itemtype="gpcm", SE=TRUE, verbose=F)
coef.gpcm.COGFLEX <- coef(results.gpcm.COGFLEX, IRTpars=TRUE, simplify=T)
items.gpcm.COGFLEX <- as.data.frame(coef.gpcm.COGFLEX$items)
Beta=gen.difficulty(results.gpcm.COGFLEX)
items.gpcm.COGFLEX=cbind(items.gpcm.COGFLEX, Beta)
theta.COGFLEX = fscores(results.gpcm.COGFLEX, method = "WLE", full.scores.SE = F)
base$COGFLEX=theta.COGFLEX
print('COGFLEX')
print(round(items.gpcm.COGFLEX,digits=2))
print("")

#FLFAMILY####
FLFAMILY=base[,7:9]
model.gpcm.FLFAMILY <- 'FLFAMILY= 1-3' 
results.gpcm.FLFAMILY <- mirt(data=FLFAMILY, model=model.gpcm.FLFAMILY, itemtype="gpcm", SE=TRUE, verbose=F)
coef.gpcm.FLFAMILY <- coef(results.gpcm.FLFAMILY, IRTpars=TRUE, simplify=T)
items.gpcm.FLFAMILY <- as.data.frame(coef.gpcm.FLFAMILY$items)
Beta=gen.difficulty(results.gpcm.FLFAMILY)
items.gpcm.FLFAMILY=cbind(items.gpcm.FLFAMILY, Beta)
theta.FLFAMILY = fscores(results.gpcm.FLFAMILY, method = "WLE", full.scores.SE = F)
base$FLFAMILY=theta.FLFAMILY
print('FLFAMILY')
print(round(items.gpcm.FLFAMILY,digits=2))
print("")

###METASPAM####
METASPAM=base[,2:3]
model.gpcm.METASPAM <- 'METASPAM= 1-2' 
results.gpcm.METASPAM <- mirt(data=METASPAM, model=model.gpcm.METASPAM, itemtype="gpcm", SE=TRUE, verbose=F)
coef.gpcm.METASPAM <- coef(results.gpcm.METASPAM, IRTpars=TRUE, simplify=T)
items.gpcm.METASPAM <- as.data.frame(coef.gpcm.METASPAM$items)
Beta=gen.difficulty(results.gpcm.METASPAM)
items.gpcm.METASPAM=cbind(items.gpcm.METASPAM, Beta)
theta.METASPAM = fscores(results.gpcm.METASPAM, method = "WLE", full.scores.SE = F)
base$METASPAM=theta.METASPAM
print('METASPAM')
print(round(items.gpcm.METASPAM,digits=2))

write.csv(base, file ="gpcm_ok.csv")