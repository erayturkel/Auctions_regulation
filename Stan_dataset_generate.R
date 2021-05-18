#Create STAN readable dataset
library(dplyr)
stan_data<-data.frame(matrix(0,ncol=10))
colnames(stan_data)<-c("id","imps","spend","male_share","female_share","twenty_share","thirty_share","fourty_share","fifty_share","party")
for(i in 1:1161){
  datarow<-two_party_ads[i,]
  impressions<-datarow$ad_imp
  id<-datarow$adv_id
  spend<-datarow$ad_spend
  male_share<-datarow$men/impressions
  female_share<-datarow$women/impressions
  twenty_share<-datarow$twenties/impressions
  thirty_share<-datarow$thirties/impressions
  fourty_share<-datarow$fourties/impressions
  fifty_share<-datarow$fifties/impressions
  party<-datarow$ad_parties
  new_row<-c(id,impressions,spend,male_share,female_share,round(twenty_share-thirty_share,3),
             round(thirty_share-fourty_share,3),
             round(fourty_share-fifty_share,3),
             round(fifty_share,3),party)
  stan_data<-rbind(stan_data,new_row)
}
stan_data<-stan_data[-1,]
stan_data%>%filter(spend>0)->stan_data
stan_data%>%filter(female_share<=1)->stan_data
stan_data%>%filter(male_share<=1)->stan_data
stan_data%>%filter(twenty_share<=1)->stan_data
stan_data%>%filter(thirty_share<=1)->stan_data
stan_data%>%filter(fourty_share<=1)->stan_data
stan_data%>%filter(fifty_share<=1)->stan_data
stan_data%>%mutate(twenty_share=ifelse(twenty_share>0,twenty_share,0))->stan_data
stan_data%>%mutate(thirty_share=ifelse(thirty_share>0,thirty_share,0))->stan_data
stan_data%>%mutate(fourty_share=ifelse(fourty_share>0,fourty_share,0))->stan_data
stan_data%>%mutate(fifty_share=ifelse(fifty_share>0,fifty_share,0))->stan_data
rm(list=setdiff(ls(),"stan_data"))



spending_vec<-as.numeric(stan_data$spend)
impressions_vec<-as.numeric(stan_data$imps)
democrat_vec<-as.numeric(stan_data$party=="d")
age_share_matrix<-(as.matrix((stan_data[,(6:9)]),ncol=4))
age_share_matrix<-matrix(as.numeric(age_share_matrix),ncol=4)
gender_share_matrix<-(as.matrix((stan_data[,(4:5)]),ncol=2))
gender_share_matrix<-matrix(as.numeric(gender_share_matrix),ncol=2)

stan_input<-list(N=1047, J=4, K=2, jj=age_share_matrix, kk=gender_share_matrix,democrat=democrat_vec,
                 spend=spending_vec,impression=impressions_vec)




