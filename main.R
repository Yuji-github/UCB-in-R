# Upper Confidence Bound 

# import data 
dataset = read.csv('Ads_CTR_Optimisation.csv')

# UCB implementation with random selections: 3 steps

# preparing
N = nrow(dataset) # get num of rows
d = ncol(dataset) # get num of lows

ads_selection = integer(0)
total_reward = 0 


for (n in 1:N) # must have 1 start
  {
    # step 1
    ad = sample(1:10, 1) #sample takes a sample of the specified size from the elements of x using either with or without replacement.
    
    # step2
    ads_selection = append(ads_selection, ad)
    reward = dataset[n, ad]
    
    # step 3 
    total_reward = total_reward + reward
  } 
  

# visualizing results
hist(ads_selection, 
     col = 'blue'
     )

# UCB implementation no random 
# step 1: 
nums_of_selection = integer(d) # d =10
sums_of_rewards = integer(d)

ads_selected = integer() # store the results
all_rewards = 0

# step 2 and 3
for (n in 1:N)
{
  ad = 0
  max_upper_bound = 0 # check maximum upper each round
  for (i in 1:d)
    {
      if(nums_of_selection[i] > 0)
      {
        # step2-1: average reward = sum of reward / num of selection
        average = sums_of_rewards[i] / nums_of_selection[i]
        
        # step2-2: confidence interval 
        confidence_interval = sqrt(3/2 * log(n) / nums_of_selection[i])
        
        # step 3-1 
        upper_bound = average + confidence_interval
      }
      else
      {
        upper_bound = 1e400 # if no one select any ads = all 0 in the row
      }
      
      if (max_upper_bound < upper_bound)
      {
        max_upper_bound = upper_bound # swap the value
        ad = i # track ad if it happens: if upper_bound = 1e400 ad will be 1
      }
  }
  # updating the values
  ads_selected = append(ads_selected, ad)
  nums_of_selection[ad] = nums_of_selection[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward 
  all_rewards = all_rewards + 1 
}

# visualizing results
hist(ads_selected, 
     col = 'blue'
)