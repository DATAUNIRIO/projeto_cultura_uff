

e1 = data.frame(table(espetaculo$deficiencia))
e2 = data.frame(table(artes_visuais$deficiencia))

av = c(Sim=3,Não=40)
esp = c(Sim=4,Não=109)
par(mfrow=c(1,2))
av = pie(av,col=c('red','blue'),main='Artes Visuais' )
esp = pie(esp,col=c('red','blue'),main='Espetáculo')


