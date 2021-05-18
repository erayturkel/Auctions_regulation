library(ggplot2)
library(tidyverse)
simulate_bids_normal<-function(n_comm, n_pol, m_comm, sd_comm, m_pol,sd_pol){
  comm_vals<-rnorm(n_comm,m_comm,sd_comm)
  pol_vals<-rnorm(n_pol,m_pol,sd_pol)
  vals_list<-list(comms=comm_vals,pols=pol_vals)
  return(vals_list)
}

sim_auctions<-function(frac_comm,n_auctions,n_comm, n_pol, m_comm, sd_comm, m_pol,sd_pol){
  q<-frac_comm
  unrestricted_rev<-rep(0,n_auctions)
  restricted_rev<-rep(0,n_auctions)
  pol_winners<-rep(0,n_auctions)
  special_auc<-rep(0,n_auctions)
  for(i in 1:n_auctions){
    bids_list<-simulate_bids_normal(n_comm,n_pol,m_comm,sd_comm,m_pol,sd_pol)  
    pol_bids<-bids_list$pols
    comm_bids<-bids_list$comms
    all_bids<-c(comm_bids,pol_bids)
    all_bids<-sort(all_bids,decreasing = TRUE)
    comm_bids<-sort(comm_bids,decreasing = TRUE)
    pol_bids<-sort(pol_bids,decreasing = TRUE)
    unrestricted_rev[i]<-all_bids[2]
    rand_draw<-runif(1)
    pol_winners[i]<-as.integer(max(all_bids)==max(pol_bids))
    if (rand_draw<q) {
      restricted_rev[i]<-comm_bids[2]
    } else {
      restricted_rev[i]<-pol_bids[2]
      special_auc[i]<-1
    }
  }
  return(list(unrestricted_rev=unrestricted_rev,restricted_rev=restricted_rev, pol_winners=pol_winners,special=special_auc))
}



calculate_loss<-function(auction_res){
  unrestricted<-auction_res$unrestricted_rev
  restricted<-auction_res$restricted_rev
  pol_winners<-auction_res$pol_winners
  specials<-auction_res$special
  percent_revenue_lost=paste0("Revenue Lost: ",as.character(round((1-(sum(restricted)/sum(unrestricted)))*100,digits=3)),"%")
  frac_pol_winners=paste0("Wins by politicians: ",as.character(round(mean(pol_winners),digits=3)*100),"%")
  frac_special=paste0("Set-aside auctions: ",as.character(round(mean(specials)*100,digits=3)), "%")
  avg_price_pol_winners=paste0("Avg price for politicians: ",as.character(round(mean(unrestricted[as.logical(pol_winners)]),digits=2)))
  avg_r_price_pol_winners=paste0("Avg price for politicians: ",as.character(round(mean(restricted[as.logical(specials)]),digits=2)))
  annot<-data.frame(text=c("Unrestricted auction setting",frac_pol_winners,avg_price_pol_winners,
                           avg_r_price_pol_winners,frac_special,percent_revenue_lost),
                    x=c(mean(unrestricted)*0.25,
                        mean(unrestricted)*0.24,
                        mean(unrestricted)*0.2,
                        mean(restricted[as.logical(specials)])*0.35,
                        mean(restricted[as.logical(specials)])*0.35,
                        mean(restricted[as.logical(specials)])*0.35),
                    y=c(mean(pol_winners[!as.logical(specials)])*5,
                        mean(pol_winners[!as.logical(specials)])*4.5,
                        mean(pol_winners[!as.logical(specials)])*4,
                        mean(pol_winners)*1,mean(pol_winners)*1.8,
                        mean(pol_winners)*1.4),
                    cols=c("blue","blue","blue","red","red","red"))
  ggplot()+geom_density(aes(unrestricted), colour="red")+
    geom_density(aes(restricted),colour="blue")+
    geom_text(data=annot, aes(x=x, y=y, label=text, colour=cols), hjust=0, size=4.5)+
    theme(
      legend.position="none"
    )+xlab(" Revenue ") + ylab(" Density ")
}

auction_res<-sim_auctions(0.95,10000,200,2,100,20,80,30)
calculate_loss(auction_res)


calculate_pct_loss<-function(auction_res){
  unrestricted<-auction_res$unrestricted_rev
  restricted<-auction_res$restricted_rev
  pol_winners<-auction_res$pol_winners
  specials<-auction_res$special
  rev_lost<-((1-(sum(restricted)/sum(unrestricted)))*100)
  return(rev_lost)
}

draw_loss_revenue_curve<-function(rangeval,numsim,n_auctions,n_comm, n_pol, m_comm, sd_comm, m_pol,sd_pol){
  holdout_vals<-seq(0,rangeval,length.out = 100)
  res_vec<-rep(0,100)
  i<-1
  for(val in holdout_vals){
    meanvec<-rep(0,numsim)
    for(num in seq(1,numsim)){
      auction_res<-sim_auctions((1-val),n_auctions,n_comm, n_pol, m_comm, sd_comm, m_pol,sd_pol)
      meanvec[num]<-calculate_pct_loss(auction_res)
    }
    res_vec[i]<-mean(meanvec)
    i<-i+1
  }
  return(res_vec)
}


  
res_vec<-draw_loss_revenue_curve(0.2,100,100,200,2,100,20,80,30)
