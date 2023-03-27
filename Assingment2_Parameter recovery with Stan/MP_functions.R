# RLAgent2 is a reinforcement learning agent with 2 learning rates: 
# alpha_w is toward winning and alpha_l is losing
RLAgent2_f <- function(previous_expectation, previous_other, feedback, alpha_w, alpha_l){
  
  if (feedback == 1){
    expectation =  previous_expectation + alpha_w * (previous_other - previous_expectation)
  } else {
    expectation =  previous_expectation + alpha_l * (previous_other - previous_expectation)
  }
  
  return(expectation)
}


# MixRLAgent uses a strategy that combines two strategies together 
# on the first half trials and second half trials respectively

MixRLAgent_f <- function(previous_expectation, previous_other, feedback, alpha1, alpha2, i) {
  #probmodel = rcat(1,c(p1,p2,p3,p4)) not sure if this can be applied
  if (i < (ntrials / 2)){ # apply strategy 1 at the first half of trails
    expectation =  previous_expectation + alpha1 * (previous_other - previous_expectation)

  }else{# apply strategy 2 at the rest half of trails
    expectation =  previous_expectation + alpha2 * (previous_other - previous_expectation)
  }
  
  return(expectation)
    
}

# two reinforcement learning agents (Rescorla Wagner model) play each other
rw_vs_rw = function(ntrials,alpha1_l,alpha1_w,alpha2_l,alpha2_w,bias1, bias2, incentive1, incentive2){
  
  
  ntrials = ntrials
  rw1 = array(NA,ntrials)
  rw2 = array(NA,ntrials)
  expectation1 = array(NA,ntrials)
  expectation2 = array(NA,ntrials)
  
  feedback_rw1 = array(NA,ntrials)
  feedback_rw2 = array(NA,ntrials)
  
  expectation1[1] = bias1
  expectation2[1] = bias2
  
  
  rw1[1] = rbinom(1,1,expectation1[1])
  rw2[1] = rbinom(1,1,expectation2[1])
  
  if(rw1[1] == rw2[1]){
    feedback_rw1[1] = 1
    feedback_rw2[1] = 1-feedback_rw1[1]
  } else{
    feedback_rw1[1] = 0
    feedback_rw2[1] = 1-feedback_rw1[1]
  }
  
  for (i in 2:ntrials){
    
    
    expectation1[i] = rw_agent2(previous_expected = expectation1[i-1],
                                previous_them = rw2[i-1],
                                alpha_w = alpha1_w,
                                alpha_l = alpha1_l,
                                feedback = feedback_rw1[i-1])
    if(incentive1 == 0){
      rw1[i] = rbinom(1,1,expectation1[i])
    }else{
      rw1[i] = rbinom(1,1,(1-expectation1[i]))
    }
    expectation2[i] = rw_agent2(previous_expected = expectation2[i-1],
                                previous_them = rw1[i-1],
                                alpha_w = alpha2_w,
                                alpha_l = alpha2_l,
                                feedback = feedback_rw2[i-1])
    
    
    if(incentive2 == 0){
      rw2[i] = rbinom(1,1,(1-expectation2[i]))
      #this overwrites the second agents decision as he would otherwise play to match which he shouldn't he tries to not match.
    }else{
      rw2[i] = rbinom(1,1,(1-(1-expectation2[i])))
      #this overwrites the second agents decision as he would otherwise play to match which he shouldn't he tries to not match.
    }
    
    
    
    if(rw1[i] == rw2[i]){
      feedback_rw1[i] = 1
      feedback_rw2[i] = 1-feedback_rw1[i]
    } else{
      feedback_rw1[i] = 0
      feedback_rw2[i] = 1-feedback_rw1[i]
    }
    
    
  }
  
  return(list(rw1 = rw1, rw2 = rw2, feedback_rw1 = feedback_rw1,feedback_rw2 = feedback_rw2, expectation1 = expectation1, expectation2 = expectation2))
  
}



# Mix agent play with RL agent
RL_vs_MIX = function(ntrials, alpha_w, alpha_l, alpha1, alpha2, bias_self, bias_other){
  ntrials = ntrials
  # self refers to the matcher, other refers to the opponent
  self = rep(NA, ntrials) # matcher
  other = rep(NA, ntrials) # opponent
  expectation_self = rep(NA, ntrials)
  expectation_other = rep(NA, ntrials)
  feedback_self = rep(NA, ntrials)
  feedback_other = rep(NA, ntrials)
  
  expectation_self[1] = bias_self
  expectation_other[1] = bias_other
  self[1] = rbinom(1,1,expectation_self[1])
  other[1] = rbinom(1,1,expectation_other[1])
  
  if (self[1] == other[1]){
    feedback_self[1] = 1
    feedback_other[1] = 0} else{
      feedback_self[1] = 0
      feedback_other[1] = 1
    }
  
  for (i in 2:ntrials){
    
    expectation_self[i] = MixRLAgent_f(alpha1 = alpha1, 
                           alpha2 = alpha2, 
                           feedback = feedback_self[i-1],
                           previous_expectation = expectation_self[i-1],
                           previous_other = other[i-1],
                           i = i)
    
    self[i] = rbinom(1,1,expectation_self[i])
    
    
    expectation_other[i] = RLAgent2_f(alpha_w = alpha_w,
                          alpha_l = alpha_l,
                          feedback = feedback_other[i-1],
                          previous_expectation = expectation_other[i-1],
                          previous_other = self[i-1])
    
    other[i] = rbinom(1,1,expectation_other[i])
    other[i] = 1 - other[i]
    
    if (self[i] == other[i]){
      feedback_self[i] = 1
      feedback_other[i] = 0} else{
        feedback_self[i] = 0
        feedback_other[i] = 1
      }
  }
  
  return(list(self = self, other = other, feedback_self = feedback_self, feedback_other = feedback_other))
}



# plot choices along ntrials
plot_choices = function(df){
  
  return(df %>% ggplot()+theme_classic()+geom_line(color = "red",aes(1:ntrials, df$self))+geom_line(color = "blue",aes(1:ntrials, df$other))+xlab("Trial")+ylab("Choice")+
           ggtitle("Plot of choices of matcher (red) and non-matcher (blue)"))
  
}


# plot the game outcomes along ntrials for singel agent
plot_cumwin = function(df){
  
  df = df %>% mutate(trials = 1:nrow(df)) %>%  mutate(cumself = cumsum(feedback_self)/seq_along(feedback_self),
                                                      cumother = cumsum(feedback_other)/seq_along(feedback_other))
  
  
  return(df %>% ggplot()+theme_classic()+geom_line(color = "red",aes(trials, cumself))+geom_line(color = "blue",aes(trials, cumother))+xlab("Trial")+ylab("Procent of wins")+
           ggtitle("Plot of Procent of wins of matcher (red) and non-matcher (blue)"))
  
  
}



# plot the game outcomes along ntrials for the group
plot_cumwin_group = function(df){
  
  df = df %>% mutate(trials = 1:nrow(df)) %>%  mutate(cumself = cumsum(feedback_self)/seq_along(feedback_self),
                                                      cumother = cumsum(feedback_other)/seq_along(feedback_other))
                                                      #cumself_sd = cumsum(self_sd)/seq_along(self_sd),
                                                      #cumother_sd = cumsum(other_sd)/seq_along(other_sd))
  
  
  return(df %>% ggplot()+
           theme_classic()+
           geom_line(color = "red",aes(trials, cumself))+
           geom_line(color = "blue",aes(trials, cumother))+
           geom_ribbon(fill = "red",aes(x=trials, y= cumself, ymin = cumself - self_sd, ymax = cumself + self_sd), alpha = 0.2) + 
           geom_ribbon(fill = "blue",aes(x=trials, y= cumother, ymin = cumother - other_sd, ymax = cumother + other_sd), alpha = 0.2) + 
           xlab("Trial")+
           ylab("Procent of wins")+
           ggtitle("Plot of Procent of wins of matcher (red) and non-matcher (blue)"))

  
}
