require(mirt)
library(ggplotify)
library(cowplot)
library(ggplot2)
base=read.csv("gpcm.csv", header = T)


###COGFLEX####
COGFLEX=base[,4:6]
model.gpcm.COGFLEX <- 'COGFLEX= 1-3' 
results.gpcm.COGFLEX <- mirt(data=COGFLEX, model=model.gpcm.COGFLEX, itemtype="gpcm", SE=TRUE, verbose=F)
coef.gpcm.COGFLEX <- coef(results.gpcm.COGFLEX, IRTpars=TRUE, simplify=T)
items.gpcm.COGFLEX <- as.data.frame(coef.gpcm.COGFLEX$items)
Beta=gen.difficulty(results.gpcm.COGFLEX)
items.gpcm.COGFLEX=cbind(items.gpcm.COGFLEX, Beta)
theta.COGFLEX = fscores(results.gpcm.COGFLEX, method = "WLE", full.scores.SE = F)
base$COGFLEX=theta.COGFLEX
print('Flexibilidade Cognitiva')
print(round(items.gpcm.COGFLEX,digits=2))
print('')

for (i in 4:6){
  
  a=plot(results.gpcm.COGFLEX, type = 'trace', which.items = (i-3), 
               main = "", par.settings = simpleTheme(lty=1,lwd=2),
               auto.key=list(points=FALSE,lines=TRUE, columns=5))
  a=as.grob(a)

  b=  ggplot(base, aes(x=as.factor(base[,i]), y=base$literacy, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "none")+
    xlab(colnames(base[i]))+ylab('Literacia Financeira')

  c=  ggplot(base, aes(x=as.factor(base[,i]), y=base$COGFLEX, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "none")+
    xlab(colnames(base[i]))+ylab('Flexibilidade Cognitiva')
    
  ggdraw() +
    draw_plot(a, 0, .5, 1, .5) +
    draw_plot(b, 0, 0, .5, .5) +
    draw_plot(c, .5, 0, .5, .5) 
 # draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 12)
  ggsave(paste0(colnames(base[i]),".pdf"), units="cm", width=20, height=20, dpi=300)
}

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

print('Discussão de temas financeiros em Família')
print(round(items.gpcm.FLFAMILY,digits=2))
print('')

for (i in 7:9){
  
  a=plot(results.gpcm.FLFAMILY, type = 'trace', which.items = (i-6), 
         main = "", par.settings = simpleTheme(lty=1,lwd=2),
         auto.key=list(points=FALSE,lines=TRUE, columns=5))
  a=as.grob(a)
  
  b=  ggplot(base, aes(x=as.factor(base[,i]), y=base$literacy, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")+
    xlab(colnames(base[i]))+ylab('Literacia Financeira')
  
  
  c=  ggplot(base, aes(x=as.factor(base[,i]), y=base$FLFAMILY, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = "none")+
                        xlab(colnames(base[i]))+ylab('Discussão de temas financeiros em Família')
  
  ggdraw() +
    draw_plot(a, 0, .5, 1, .5) +
    draw_plot(b, 0, 0, .5, .5) +
    draw_plot(c, .5, 0, .5, .5) 
  # draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 12)
  ggsave(paste0(colnames(base[i]),".pdf"), units="cm", width=20, height=20, dpi=300)
}

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

print('Metacognição na deteção de spam')
print(round(items.gpcm.METASPAM,digits=2))

for (i in 2:3){
  
  a=plot(results.gpcm.COGFLEX, type = 'trace', which.items = (i-1), 
         main = "", par.settings = simpleTheme(lty=1,lwd=2),
         auto.key=list(points=FALSE,lines=TRUE, columns=5))
  a=as.grob(a)
  
  b=  ggplot(base, aes(x=as.factor(base[,i]), y=base$literacy, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "none")+
    xlab(colnames(base[i]))+ylab('Literacia Financeira')
  
  c=  ggplot(base, aes(x=as.factor(base[,i]), y=base$METASPAM, fill=as.factor(base[,i]))) +
    geom_boxplot(outlier.size = 0) +geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_manual(values=c("#0080FF", "#FF00FF", "#006400", "#E30613", "#FFA500", "#00FF00"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "none")+
    xlab(colnames(base[i]))+ylab('Metacognição na deteção de spam')
  
  ggdraw() +
    draw_plot(a, 0, .5, 1, .5) +
    draw_plot(b, 0, 0, .5, .5) +
    draw_plot(c, .5, 0, .5, .5) 
  # draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 12)
  ggsave(paste0(colnames(base[i]),".pdf"), units="cm", width=20, height=20, dpi=300)
}

write.csv(base, file ="gpcm_ok.csv")