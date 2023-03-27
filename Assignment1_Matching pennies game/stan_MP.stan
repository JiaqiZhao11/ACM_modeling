// Matching Pennies game agent

data {
  int <lower=1> n; // trials
  array[n] int h; // Choices
  // vector<lower=0,upper=1>[n] memory; 
  // array[n] int self;
  array[n] int other;

}

parameters{
  real bias; // bias of the agent playing right without information of the opponent
  real beta; // how strong the memory would affect the decision
}

transformed parameters{
  vector[n] memory; // it is also an array, but stan needs it to be a vector
  for (t in 1:n) {
    if (t == 1){memory[t] = 0.5;}
    if (t < n){
      memory[t+1] = memory[t] + ((other[t] - memory[t]) / t);
      if (memory[t + 1] == 0){memory[t + 1] = 0.01;} // 0 or 1 will get an infinite value
      if (memory[t + 1] == 1){memory[t + 1] = 0.99;}
    }
  }
}

model {
  //target += beta_lpdf(bias|1,1) // target is a function, in this cace it is a distribution
  //target += bernoulli_lpmf(h|bias) // usually use Bernoulli when sample for 1 trial 
  // target += binomial_lpdf(h|1, bias) // this is the alternative way using binomial distribution
  
  // alternative way using a normal distribution for priors
  target += normal_lpdf(bias|0,0.3);
  target += normal_lpdf(beta|0,0.5);
  // choices based on the model consisting a bernoulli distribution of a rate of theta
  //target += bernoulli_logit_lpmf(h|theta)
  
  // agent model that keeps track of the memory
  for (t in 1:n) {
    target += bernoulli_lpmf(h[t] | bias + beta * logit(memory[t]));
    
  }
  
  // if we use the RW model, we need a for loop 
}

generated quantities{
  real bias_prior;
  real beta_prior;
  int<lower=0, upper=n> prior_preds5;
  int<lower=0, upper=n> post_preds5;
  int<lower=0, upper=n> prior_preds7;
  int<lower=0, upper=n> post_preds7;
  int<lower=0, upper=n> prior_preds9;
  int<lower=0, upper=n> post_preds9;
  
  bias_prior = normal_rng(0, 0.3);
  beta_prior = normal_rng(0, 0.5);
  prior_preds5 = binomial_rng(n, inv_logit(bias_prior + beta_prior * logit(0.5)));
  prior_preds7 = binomial_rng(n, inv_logit(bias_prior + beta_prior * logit(0.7)));
  prior_preds9 = binomial_rng(n, inv_logit(bias_prior + beta_prior * logit(0.9)));
  post_preds5 = binomial_rng(n, inv_logit(bias + beta * logit(0.5)));
  post_preds7 = binomial_rng(n, inv_logit(bias + beta * logit(0.7)));
  post_preds9 = binomial_rng(n, inv_logit(bias + beta * logit(0.9)));
}







