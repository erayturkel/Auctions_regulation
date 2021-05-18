data {
  int<lower=1> N;              // number of ad campaigns
  int<lower=1> J;              // number of age groups
  int<lower=1> K;              // number of gender groups
  real<lower=0,upper=1> jj[J,N];  // age group shares for ad campaign n
  real<lower=0,upper=1> kk[K,N];  // gender group shares for ad campaign n
  int<lower=0,upper=1> democrat[N];   // party ID for ad campaign N
  real<lower=0> spend[N];   // spent amount for ad campaign n
  real<lower=0> impression[N];   // impressions for ad campaign n
}
parameters {
  real<lower=0> mu_d_female;
  real<lower=0> mu_d_male;
  real<lower=0> mu_d_twenties;
  real<lower=0> mu_d_thirties;
  real<lower=0> mu_d_fourties;
  real<lower=0> mu_d_fifties;
  real<lower=0> mu_r_female;
  real<lower=0> mu_r_male;
  real<lower=0> mu_r_twenties;
  real<lower=0> mu_r_thirties;
  real<lower=0> mu_r_fourties;
  real<lower=0> mu_r_fifties;
}
transformed parameters {
  real impressions_age_gender_share[J * K, N];
  for(k in 1:K){ //gender
    for(j in 1:J){ //agegroup
      for(i in 1:N){
        impressions_age_gender_share[j * k, i]=impression[i]*jj[j,i]*kk[k,i]
      }
    }
  }
}
model {
  mu_d_female ~ uniform(0.5,0.01);
  mu_d_male ~ uniform(0.5,0.01);
  mu_d_twenties ~ uniform(0.5,0.01);
  mu_d_thirties ~ uniform(0.5,0.01);
  mu_d_fourties ~ uniform(0.5,0.01);
  mu_d_fifties ~ uniform(0.5,0.01);
  mu_r_female ~ uniform(0.5,0.01);
  mu_r_male ~ uniform(0.5,0.01);
  mu_r_twenties ~ uniform(0.5,0.01);
  mu_r_thirties ~ uniform(0.5,0.01);
  mu_r_fourties ~ uniform(0.5,0.01);
  mu_r_fifties ~ uniform(0.5,0.01);
  for(k in 1:K){ //gender
    for(j in 1:J){ //agegroup
      for(i in 1:N){
        if (democrat[i] == 1) {
          if(k==1){
            if(j==1){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_female+mu_d_twenties);
            }
            else if(j==2){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_female+mu_d_thirties);
            }
            else if(j==3){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_female+mu_d_fourties);
            }
            else{
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_female+mu_d_fifties);
            }
          }
          else{
            if(j==1){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_male+mu_d_twenties);
            }
            else if(j==2){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_male+mu_d_thirties);
            }
            else if(j==3){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_male+mu_d_fourties);
            }
            else{
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_d_male+mu_d_fifties);
            }            
          }
        }
        else {
          if(k==1){
            if(j==1){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_female+mu_r_twenties);
            }
            else if(j==2){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_female+mu_r_thirties);
            }
            else if(j==3){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_female+mu_r_fourties);
            }
            else{
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_female+mu_r_fifties);
            }
          }
          else{
            if(j==1){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_male+mu_r_twenties);
            }
            else if(j==2){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_male+mu_r_thirties);
            }
            else if(j==3){
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_male+mu_r_fourties);
            }
            else{
              target += frechet_lpdf(spend[i] / impressions_age_gender_share[j * k, i]| 2, mu_r_male+mu_r_fifties);
            }            
          }
        }
        target += -beta_lpdf(kk[k,i]|kk[k,i],1-kk[k,i])-beta_lpdf(jj[j,i]|jj[j,i],1-jj[j,i])
        }
      }
    }
  }
