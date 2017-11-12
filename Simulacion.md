
#Este es un ejemplo de cómo podríamos simular la llegada de clientes,  y al efectuar
#un plan de acción en cuanto afectaría en términos del  tiempo  de espera.
#En este ejemplo simularemos una oficina en donde supondremos que hay dos asesores,
#y un tiempo de atención diferente por servicio.
TPE<-0
for(W in 1:100){
CC<-sample(100:150,20)

##Hora de llegada delo clientes
k=1
Hora<-sample(9:17,CC[k],replace = TRUE)
Mknutos<-sample(0:60,CC[k],replace = TRUE)
FS<-Hora*3600+Mknutos*60

#Tipo de Servicio: si solicitó ficho para caja o asesoría comercial.
TS<-sample(c(1,2,3,4,5),CC[k],replace = TRUE,prob=c(1,2,3,4,5))

##Tiempo de atencion segun servicio

TA<-round(60+rexp(CC[1],1/(1+TS^1.5))*60)

Datos<-data.frame(Hora,Mknutos,FS,SC,TS,TA)
Datos<-Datos[order(FS),]
##row.names(Datos)=NULL
boxplot(TA~TS)

NPA<-0;NPDE<-0;TC<-0
##NCE numero de pasientes que les toco esperar TE tiempo de espera
##NCE numero de pasientes que les toco esperar, Hora en el pasiente i
NCE=0;TE=0
S1=0;S2=0;Hora=0;L1=0;L2=0
FS<-Datos$FS
TA<-Datos$TA

for(i in 1:(CC[1]-1)){
  Hora=FS[i]
  if(S1==0){ 
    S1=1
    L1=FS[i]+TA[i]
    TE[i]=0
  }else{
    if(S2==0){
      S2=1
      L2=FS[i]+TA[i]
      TE[i]=0
    }else{
        NCE=NCE+1
        TE[i]=min(L1,L2)-FS[i]
        ##Si todo ocupado espera y lo atiende el primero que se desocupe
        if(L1<L2){L1=L1+TA[i]}else{L2=L2+TA[i]}
  
      }
    }
  
  if(S1==1 && L1<=(FS[i+1])){S1=0}
  if(S2==1 && L2<=(FS[i+1])){S2=0}
  
}

##Cual fue el tiempo de espera promedio?
TPE[W]=mean(TE)

}



