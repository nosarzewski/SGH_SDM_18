# Buliding decision trees for business problems
We will be building our own decision trees for business problems in `Silver Decisions`. Please go to:  
http://silverdecisions.pl/  
Documentation can be found here:  
https://github.com/SilverDecisions/SilverDecisions/wiki  
## Case  
We are a company who is selling insurances. We are intrested in offering following product - firstly we can sell life insurance and then additionally we can offer car insurance.  
The assumptions are following:  
* We decide to which client we want to send an offer for life insurance - sending an offer costs `$10`.
* The probability of a client being interested in our offer and buying a life insurance is `30%`. Let's assume that price of an insurance is proportional to the age of a client (which roughly makes sense, the older the person the higher risk of dying) - for a 20-year-old person price is `$20`, for 40 years `$40`, etc. In other words our price is dependant on variable `age`.
* If a client did buy a life insurance we can offer them additional car insurance. Since car insurance is much more complicated product and needs to be prepared for each client seperately, costs of preparing an offer are higher and amount to `$16`.
* Once again our client can accept the offer with probability of `30%`. The price of car insurance is inversely proportional to age (the older our client, the more experienced a driver they are) and given by a function `price=100-age`.  

We need to build a decision tree using `Silver Decisions` and come up with a marketing plan for clients in `age` `20`, `40` and `60`.
