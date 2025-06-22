paper: THE NETWORK ORIGINS OF AGGREGATE FLUCTUATIONS

#### summary
The possibility that significant aggregate fluctuations may originate from microeconomic shocks to firms or disaggregated sectors was long been discarded in macroeconomics due to a “diversification argument.” As argued by Lucas (1977), such microeconomic shocks would average out, and thus, would only have negligible aggregate effects.

in an economy consisting of $n$ sectors hit by INDIPENDENT shocks, aggregate fluctuations would have magnitude proportional to $1/\sqrt{n}$ (that is, $n^{-0.5}$) , which is small since $n$ is typically high

this argument ignores the presence of interconnections between different sectors as  potential propagators of shocks - cascade effect due to interconnections

**example**
	fig 1: fully connected network - small fluctuation effects
	fig 2: star motif - as $n$ increases, sectoral shocks are not averaged out: shocks on the central node propagate strongly to the rest of the economy

<mark style="background: #FFF3A3A6;">TODO</mark>: plot graphs!!!! like fig 3 in the paper...that is super cool

dependency on the *topology* of the network! 
* theorem 2 (1 order connections) 
* theorem 3 (second order connections)
* theorem 4 (for balanced networks it's correct that sectoral shocks average out at the rate $\sqrt{n}$).

See section 4 for applications on the u.s. economy

***
**NOTATION**
* $a_n=O(b_n)$ if $\limsup_{n\rightarrow\infty} a_n/b_n<\infty$  
* $a_n =\Omega(b_n)$ if $\liminf_{n\rightarrow\infty}a_n/b_n>0$
* $a_n = \Theta(b_n)$ if $a_n =\Omega(b_n)$ and $a_n=O(b_n)$
***

$x_i = z_i^\alpha l_i^\alpha\prod_{j=1}^n x_{ij}^{(1-\alpha)w_{ij}}$

economy: $\mathcal{E}=(\mathcal{I}, W, \{F_i\}_{i\in\mathcal{I}})$

* $\mathcal{I}=\{1,2,\dots n\}$ set of sectors 
* $w_{ij}$ entries of IO matrix (normalized) $W$, $w_{ij}>0$ if j is an *input supplier* to i ($j\longrightarrow i$) - assumption: $\sum_{j=1}^n w_{ij} = 1$
* $z_i$ shock on sector $i$, $\varepsilon_i=\log(z_i)$ with distribution $F_i$

(outer) degree of sector i: $d_i=\sum_{j=1}^n w_{ij}$

aggregate volatility: st. deviation of aggregate output

logarithm of real value added, which we call *aggregate output*, is a linear combination of log sectoral shocks with coefficients determined by elements of the influence vector.
$$y=\log(GDP)=v'\varepsilon$$
with $\varepsilon=[\varepsilon_1,\varepsilon_2,\dots,\varepsilon_n]$ and $v=\frac{\alpha}{n}[I-(1-\alpha)W]^{-1}\mathcal{1}$

yeah I mean this is not very clear but at least we see that the aggregate output depends on the intersectional network of the economy through the Leontief inverse $[I-(1-\alpha)W]^{-1}$

if the log sectoral shocks variances remain bounded then we have that the aggregate volatility (st. deviation of aggreate output)
$$(\text{var }y_n)^{1/2}=\sqrt{\sum_{i=1}^n\sigma_{in}^2v_{in}^2}=O(\|{v_n}\|_2)$$
this relationship shows that the rate of decay of aggregate volatility depends on $v_n$ which depends on the structure of the interconnection network.


#### theorem 2
Given an economy $\mathcal{E}_n$ with sectoral degrees $d_1^n, d_2^n, \dots$, we define the coefficient of variation
$$CV_n=\frac {1}{\bar{d_n}}\left[\frac{1}{n-1}\sum_{i=1}^n (d_i^n-\bar{d_n})^2\right]$$
where $\bar{d_n} = (\sum_{i=1}^n d_i^n)/n$

aggregate volatility satisfies $(\text{var } y_n)^{1/2}=\Omega\left(\frac{1+CV_n}{\sqrt{n}}\right)$

*consequently, if the degree sequence exhibits high variability, as measured by the coefficient of variation, then there is also high variability in the effect of sector-specific shocks on the aggregate output. Such asymmetries in the roles of sectors imply a slower day for aggregate volatility.

also, aggregate volatility is higher in economies whose corresponding degree sequences have a heavier tail:

in the def 2 there's a very complicated definition of what they mean with *power law degree sequence*, but we syntetize it by saying that given $$P_n(x):=\frac 1 n |\{i\in \mathcal{I}_n: d^n_i>x\}|$$ for all $n$, then this follows the law  $$\log P_n(x)\approx\gamma_0 - \beta \log(x)$$ for a sufficiently large $x$ (CAUSE WE CARE ONLY ABOUT THE TAIL!!!!).

Lower $\beta$ correspond to heavier tails $\Longrightarrow$ larger variations in the degree sequence

#### Corollary 1
consider a sequence of economies $\mathcal{E}_n$ with power law degree sequence and shape parameter $\beta\in(1,2)$ then

$(\text{var }y_n)^{1/2}=\Omega(n^{-(\beta -1)/\beta -\delta})$

therefore, if we have an heavier tail (i.e., lower beta) then aggregate volatility decreases at a much *slower* rate than the one predicted by the standard diversification argument!
#### Theorem 3
$$\tau_2(W_n)=\sum_i\sum_{j\neq i}\sum_{k\neq i,j} w_{ji}^n w_{ki}^n d_j^n d_k^n$$
note that $τ_2$ takes higher values when high-degree sectors share suppliers with other high-degree sectors, as opposed to low-degree ones

Given a sequence of economies aggregate volatility satisfies 
$$(\text{var } y_n)^{1/2}=\Omega\left(\frac {1}{\sqrt n} + \frac {CV_n}{\sqrt n} + \frac {\sqrt{\tau_2(W_n)}}{n}\right)$$
NOTE: it's a refinement of theorem 2
#### Corollary 2
second order degree sequence: $q_i^n:=\sum_{j=1}^n d_j^n w_{ji}^n$

suppose this follows for all n a power low with shape parameter $\zeta\in(1,2)$, then for all $\delta>0$
$(\text{var }y_n)^{1/2}=\Omega(n^{-(\zeta -1)/\zeta -\delta})$

#### Theorem 4
given a balanced network, we have the "standard" aggregate volatility evolution as $1/\sqrt{n}$

> A noteworthy corollary to this theorem is that many network structures that are often considered to be “fragile,” such as the ring, have exactly the same asymptotic behavior as a fully connected network as far as aggregate volatility is concerned. In particular, the “sparseness” of the input–output matrix has no impact on this asymptotic behavior.


***
### Procedure used in the paper for the U.S. economy
they used super fine IO tables for U.S. from 1972 to 2002. the typical (ij) entry captures the value of spending on commodity i per dollar of production of commodity j (i is the $input$ for producing j).

REMEMBER: direct requirements tables $W_n$, where the typical $(i, j)$ entry captures the value of spending on commodity i per dollar of production of commodity j

* **step 0**: normalization: *we normalized each entry of the direct requirements tables by the total input requirement of the corresponding sector* (so I guess normalization by columns?)
* **step 1**: observation of the variation in the weighted in-degrees for each sector: plot weighted in-degree/empirical density: count the percentage of sectors that are within one standard deviation of the mean input share.
* **step 2**: observation of the variation in the weighted 1/2 order out-degrees for each sector: plot weighted 1/2 order out-degree/empirical density - is it skewed? if yes, Such skewed distributions are indicative of presence of commodities that are general purpose inputs used by many other sectors and major suppliers to sectors that produce the general purpose inputs. which are these sectors? (comment 25 in the paper)
* **step 3** plot the empirical counter-cumulative distribution functions (i.e., 1 minus the empirical cumulative distribution functions) of the first-or3der and second-order degrees on a log–log scale - on x: degree, on y: ECDF. verify this curve (**which is equivalent to $P_n(x)$**) follows a power law distribution. theoretically, we should have a straight line in the log-log plot above ON THE TAIL: with tail, we mean we take the top 20% largest sectors in terms of d and q.
* **step 4**: Gabaix–Ibragimov’s method to estimate shapes
* **step 5**: use this and the theorems to check aggregate volatility decay. for example, having estimated $\zeta=1.18$, means that for all $\delta>0$ we have $(\text{var }y_n)^{1/2}=\Omega(n^{-(\zeta -1)/\zeta -\delta})$, which means that aggregate volatility decays slower than $n^{-(\zeta -1)/\zeta}\approx n^{-0.15}$ - WE HAVE AN UPPER BOUND FOR DECAY OF AGGREGATE VOLATILITY
* for a sequence of economies in which the empirical distributions of both first- and second-order degrees have power law tails with exponents β and ζ, the tighter bound for the decay rate of aggregate volatility is determined by min{ β, ζ }


### IMPORTANT NOTE
why the shape parameter of the first and second order degrees must be bounded between 1 and 2?

simply because if it's in that range then power-law exponents in this interval create heavy tails that have finite mean but very very high variance! If the shape parameter ($\beta$ or $\zeta$) is greater than 2, then the tail is "soft", and the variance is low enough to apply the LAW OF LARGE NUMBERS, which assures that the aggregate volatility converges to an average value *with speed $1/ \sqrt{n}$* 






