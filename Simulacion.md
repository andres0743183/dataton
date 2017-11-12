#Este es un ejemplo de cómo podríamos simular la llegada de clientes, y al efectuar
#un plan de acción en cuanto afectaría en términos del tiempo  de espera.
#En este ejemplo simularemos una oficina en donde supondremos que hay dos asesores,
#y un tiempo de atención diferente por servicio.

TPE<-0
for(W in 1:100){
  CC<-sample(100:150,1)
  
  #Hora de llegada delo clientes, toda esta información simulada se podría realizar de la 
  #distribución real  de los datos
  k=1
  Hora<-sample(9:17,CC,replace = TRUE)
  Mknutos<-sample(0:60,CC,replace = TRUE)
  FS<-Hora*3600+Mknutos*60
  
  #Tipo de Servicio: si solicitó ficho para Asesoría General, Básica, Prioritaria o Caja General Divisas
  TS<-sample(c(1,2,3,4,5),CC[k],replace = TRUE,prob=c(1,2,3,4,5))
  
  ##Tiempo de atención según servicio
  
  TA<-round(60+rexp(CC[1],1/(1+TS^1.5))*60)
  
  #Orden de los datos
  
  Datos<-data.frame(Hora,Mknutos,FS,TS,TA)
  Datos<-Datos[order(FS),]
  #Grafico  de tipo  de servicio vs tiempo de atencion
  #boxplot(TA~TS)
  
  #Iniciación de variables
  NPA<-0;
  NPDE<-0;
  TC<-0
  NCE=0;
  TE=0
  S1=0;
  S2=0;
  Hora=0;
  L1=0;
  L2=0
  FS<-Datos$FS
  TA<-Datos$TA
  
  #Simulacion
  
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
  
  #Calculo del tiempo promedio en cola 
  TPE[W]=mean(TE)
  
}

#Grafico del tiempo promedio en cola. Corrido a partir de 100 simulaciones del mismo evento
hist(TPE, xlab="Tiempo promedio", col=3, main = "Tiempo promedio de la simulación")
     
