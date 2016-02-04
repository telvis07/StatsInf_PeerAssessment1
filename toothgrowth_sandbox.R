library(datasets)
library(ggplot2)
library(dplyr)

data("ToothGrowth")

tooth_summary_stats <- function(){
  by_supp <- group_by(ToothGrowth, supp)
  summary(by_supp)
  
}

plot_tooth_by_supp <- function(){
  mean_supp_dose <- dcast(melt(ToothGrowth, id=c("supp", "dose"), 
                               measure.vars = "len"), 
                          supp+dose ~ variable, mean)
  
  g <- ggplot(mean_supp_dose, aes(x=factor(dose), 
                                  y=len, 
                                  group=supp, 
                                  fill=supp)) + 
    geom_bar(stat="identity") + 
    facet_grid(. ~ supp) + 
    labs(x="Dose") +
    labs(y="Tooth Length") +
    labs(title="Tooth Length by Supplement + Dose ")
    
  print(g)
}

conf_by_supp <- function(){
  rbind(
    c(0.5, t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data=subset(ToothGrowth, dose==0.5))$conf),
    c(1.0, t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data=subset(ToothGrowth, dose==1.0))$conf),
    c(2.0, t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data=subset(ToothGrowth, dose==2.0))$conf)
  )
}

conf_by_dose <- function() {
  ret <- matrix(nrow=0, ncol=5)
  for (supp_ in c("VC", "OJ")){
    print(supp_)
    ret <- rbind(
        ret,
        c(supp_,
          0.5,
          1.0,
          t.test(subset(ToothGrowth, dose==1.0 & supp==supp_)$len, 
                 subset(ToothGrowth, dose==0.5 & supp==supp_)$len, 
                 var.equal = FALSE)$conf),
        c(supp_,
          1.0,
          2.0,
          t.test(subset(ToothGrowth, dose==2.0 & supp==supp_)$len, 
                 subset(ToothGrowth, dose==1.0 & supp==supp_)$len, 
                 var.equal = FALSE)$conf)
      )
  }
  ret
}