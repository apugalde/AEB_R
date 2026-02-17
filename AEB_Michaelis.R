
# Plot michaelis menten curves with different parameters
mmeq <- function(E,s,kcat,km) {
  (E * kcat *s) / (km + s)
}

x <- seq(0,100,0.1)
y <- lapply(enzconc,mmeq,s=x,kcat=100,km=10)
df <- data.frame('Enz'=rep(enzconc,each=length(x)),
                 'x' = rep(x,length(enzconc)),
                 'y'= do.call('c',y))
ggplot(df,aes(x=x,y=y,color=factor(Enz)))+geom_line()+
  geom_vline(xintercept=10,linetype=2)+
  theme_bw(12)


# Plot rxn progress at different enzymes concentrations
require(ggplot2)
rxprog <- function(E,time,S,kcat,km){
  S*(1-exp(-(kcat*E*time/km)))
}
enzconc <- c(0.1,0.2,0.3,0.4)/1000
time <- seq(0,60*10,0.1)
y <- lapply(enzconc,rxprog,time=time,S=100,kcat=100,km=10)
df <- data.frame('Enz'=rep(enzconc,each=length(time)),
                 'x' = rep(time,length(enzconc)),
                 'y'= do.call('c',y))
ggplot(df,aes(x=x,y=y,color=factor(Enz)))+
  geom_line()+theme_bw()+xlab(NULL)+ylab(NULL)

# Plot reaction progress with different amounts of inicial sustrate
require(ggplot2)
# Function for the reaction progress
  rxprog <- function(E,time,S,kcat,km){
    S*(1-exp(-(kcat*E*time/km)))
  }

susconc <- c(0.1,0.2,0.3,0.4) # sustrate concentrations
time <- seq(0,60*60,0.1)

y <- lapply(susconc, function(s){
  rxprog(E=0.0001,time,S=s,kcat=100,km=10)
})
df <- data.frame('S0'=rep(susconc,each=length(time)),
                 'x' = rep(time,length(susconc)),
                 'y'= do.call('c',y))
ggplot(df,aes(x=x,y=y,color=factor(S0)))+
  geom_line()+theme_bw()+xlab(NULL)+ylab(NULL)+
coord_cartesian(xlim=c(0,300),ylim=c(0,0.15))
