functions{
  // assimetric laplace cumulative distribution
  real Fa(real x, real p) {
    return x < 0 ?  p*exp(x*(1-p)) : p + (1-p)*(1-exp(-x*p));
  }

  // vetorized Fa function
  vector Fa_vec(vector x, real p) {
    vector[num_elements(x)] probs;
    for(i in 1:num_elements(x)) {
      probs[i] = Fa(x[i], p);
    }
    return probs;
  }

  // transform float in int  (source: stack overflow)
  int to_int(real x, int min_val, int max_val){
    int range = (max_val - min_val+1)/2;
    int mid_pt = min_val + range;
    int out;
    while(range > 0) {
      if(x == mid_pt){
        out = mid_pt;
        range = 0;
      } else {
        range =  (range+1)/2;
        mid_pt = x > mid_pt ? mid_pt + range: mid_pt - range;
      }
    }
    return out;
  }

  // inferior gamma to the likelihooh
  vector lower_cut(vector x, vector cuts, int J) {
    vector[num_elements(x)] val;
    for(i in 1:num_elements(x)) {
      //primeiro valor representado por -inf
      if(x[i] == 1) val[i] = negative_infinity();
      //segundo valor: 0
      else if(x[i] == 2) val[i] = 0;
      else val[i] = cuts[to_int(x[i], 0, J+1)-2];
    }
    return val;
  }

  // superior gamma to the likelihood
  vector upper_cut(vector x, vector cuts, int J) {
    vector[num_elements(x)] val;
    for(i in 1:num_elements(x)) {
      if(x[i] == 1) val[i] = 0;
      else if(x[i] == J) val[i] = positive_infinity();
      else val[i] = cuts[to_int(x[i], 0, J+1)-1];
    }
    return val;
  }

  // final log likelihood
  vector logvero(vector y, matrix x, vector beta, vector delta, real p, int J) {
    vector[num_elements(y)] logv;
    vector[num_elements(delta)] cuts = cumulative_sum(exp(delta));
    vector[num_elements(y)] pred = x*beta;
    vector[num_elements(y)] ucut = upper_cut(y, cuts, J);
    vector[num_elements(y)] lcut = lower_cut(y, cuts, J);
    logv = log(Fa_vec(ucut - pred, p) - Fa_vec(lcut - pred, p));
    return logv;

  }



}

// data
data{
  int k; // number of categories in the response
  int n; // number of observations
  int p; // number of covariates
  real<lower=0, upper = 1> q; //quantile
  vector<lower=1, upper = k>[n] y; // response variavle
  matrix[n, p] x; // covariates
  real beta_scale;
  real delta_scale;
}


parameters {
  vector[p] beta; // model coefs
  vector[k-2] delta; // deltas
}
transformed parameters {
  // gamma as a function of deltas
  vector[k-2] gamma = cumulative_sum(exp(delta));
}

model {
  //likelihood
  vector[num_elements(y)] logv_obs;
  logv_obs = logvero(y, x, beta, delta, q, k);
  target += sum(logv_obs);
  //priors
  beta ~ normal(0, beta_scale);
  delta ~ normal(0, delta_scale);

}

generated quantities {
  vector[n] log_lik;
  //posterior loglike
  log_lik = logvero(y, x, beta, delta, q, k);
}


