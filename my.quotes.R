
# source("dt.R")
# library(statquotes)
# quotes %>% names
# 
# statquote(topic="science")
# quote_cloud()

if(T) {
  qq <- 2222
  qq[1] = "Happiness is not something ready made. It comes from your own actions (Dalai Lama)"
  qq[2] = "We need to learn to want what we have, not to have what we want, in order to get stable and steady happiness (Dalai Lama)"
  qq[3] = "It is under the greatest adversity that there exists the greatest potential for doing good, both for oneself and others (Dalai Lama)"
  qq[4] = "My religion is very simple. My religion is kindness (Dalai Lama)"
  qq[5] = "Give the ones you love wings to fly, roots to come back, and reasons to stay (Dalai Lama)"
  qq[6] = "If you want others to be happy, practice compassion. If you want to be happy, practice compassion (Dalai Lama)"
  qq[7] = "Be kind whenever possible. It is always possible (Dalai Lama)"
  qq[8] = "Sometimes one creates a dynamic impression by saying something, and sometimes one creates as significant an impression by remaining silent (Dalai Lama)"
  qq[9] = "Remember that sometimes not getting what you want is a wonderful stroke of luck (Dalai Lama)"
  qq[10] = "The more you are motivated by love, the more fearless and free your action will be (Dalai Lama)"
  qq[11] = "Just one small positive thought in the morning can change your whole day (Dalai Lama)"
  qq[12] = "Choose to be optimistic, it feels better (Dalai Lama)"
  qq[13] = "An open heart is an open mind (Dalai Lama)"
  qq[14] = "If a problem is fixable, if a situation is such that you can do something about it, then there is no need to worry. If it's not fixable, then there is no help in worrying. There is no benefit in worrying whatsoever (Dalai Lama)"
  qq[15] = "This is my simple religion. No need for temples. No need for complicated philosophy. Your own mind, your own heart is the temple. Your philosophy is simple kindness (Dalai Lama)"
  qq[16] = "As you breathe in, cherish yourself. As you breathe out, cherish all Beings (Dalai Lama)"
  qq[17] = "The way to change others? minds is with affection, and not anger (Dalai Lama)"
  qq[18] = "Love and Compassion are the true religions to me. But to develop this, we do not need to believe in any religion (Dalai Lama)"
  qq[19] = "Whether one is rich or poor, educated or illiterate, religious or non-believing, man or woman, black, white, or brown, we are all the same. Physically, emotionally, and mentally, we are all equal. ... On this fundamental level, religion, ethnicity, culture, and language make no difference (Dalai Lama)"
  qq[20] = "The goal is not to be better than the other man, but your previous self (Dalai Lama)"
  
}
d7.getQuote <- function(rand = 999) {
  qq[ runif(1, 1, length(qq)) ]
}
