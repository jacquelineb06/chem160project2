alpham<-0.000006 #(alpha males)
alphaf<-0.0000009 #(alpha females)

gammam<-0.05 #(day-1)
gammaf<-0.007 #(day-1)

Sm<-14000
Sf<-9000
Im<-1000
If<-1000


Sm.hist<-c()
Im.hist<-c()
Sf.hist<-c()
If.hist<-c()


for (steps in 1:2000) {
    Sm.hist[steps]<-Sm
    Im.hist[steps]<-Im
    Sf.hist[steps]<-Sf   
    If.hist[steps]<-If
    delta.Sm<-(gammam*Im-alpham*Sm*If)
    delta.Im<-alpham*Sm*If-gammam*Im
    delta.Sf<-(gammaf*If-alphaf*Sf*Im)
    delta.If<-alphaf*Sf*Im-gammaf*If
   
    
    Sm<-Sm+delta.Sm
    Im<-Im+delta.Im
    Sf<-Sf+delta.Sf
    If<-If+delta.If
 


    ## Ensure S,I, > 0
    Sm<-max(Sm,0) 
    Im<-max(Im,0)
    Sf<-max(Sf,0)
    If<-max(If,0)
    
}
plot(Im.hist, main = "SIS model of Genders",type="l", lwd=2)
lines(If.hist,col=4, lwd=2)