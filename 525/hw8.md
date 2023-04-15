### question 2
* a
  * (i) and (iii) are contrasts. 
  * In (i), c1 = 1, c2 = 3, c3 = 4. 
  * In (iii), c1 = ⅓, c2 = ⅓, c3 = ⅓, c4 = -1
* b
  * An unbiased estimator for each of the linear combinations: $ \hat Li = ΣCi* \hat Yi$.
  * The estimated variance of each estimator:$ S^2 (\hat L)  = MSE*Σ(\frac{Ci^2}{ni})$
  * $ (\hat Li) = Y1*1 + Y2*3 + Y3*4 $
    * $ S^2(\hat Li) = MSE*Σ(1+9+16)/n = \frac{26*MSE}{n} $
  * $ (\hat Li) = Y1*0.3 + Y2*0.5 + Y3*0.1 + Y4*0.1 $
    * $ S^2(\hat Li) = MSE*Σ(0.09+0.25+0.01+0.01)/n = \frac{0.36*MSE}{n} $
  * $ (\hat Li) = Y1*⅓ + Y2*⅓ + Y3*⅓ + Y4*(-1) $
    * $ S^2(\hat Li) + MSE*Σ(\frac{1}{9}+\frac{1}{9}+\frac{1}{9}+1)/n = \frac{1.333*MSE}{n} $

### question 3
* b
  * For the mean fill in machine 1, 
    t(1-(0.05/2), 114) = 1.981
    CI(0.95, 114) = [0.0735 ± 1.981(.0394)] = [-.005, .152]
    ××[0.07350 ± 1.9810.19252*0.039351] = [0.07350 ± 0.007575]
* c
  * $\hat D = \bar{Y}_{2}+\bar{Y}_{1} = 0.1905 - 0.0735 = 0.1170 $
  * $ S^2 (\hat D) = 0.4578*  ( \frac{1}{20} + \frac{1}{20}) = 0.04578$ 
  * $ t(.975 ;114) = .1981 $
  * $ CI(0.95, 114) = 0.1170 ± 1.981(0.04578) = [0.0263, 0.2076]$
* e
  * Using $ \bar{Y} $ as an estimate of $ μ_{i}, $
    $\hat D_{1} = \bar{Y}_{1}-\bar{Y}_{4} = 0.375-0.3655=-0.2920$
    $\hat D_{2} = \bar{Y}_{1}-\bar{Y}_{5} = 0.735-0.1250=-0.0515$
    $\hat D_{3} = \bar{Y}_{4}-\bar{Y}_{5} = 0.3655-0.1250=0.2405$
    -> $ S(\hat D_{i}) =0.0557, i=1,2,3, B=t(0.9833, 114) = 2.178 $
    -> $ \frac{\hat D}{S(D_{1})} =-5.240, \frac{\hat D}{S(D_{2})} =-0.925, \frac{\hat D}{S(D_{3})} =4.32$
    ＊ therefore, we reject $H_{0}$ and accept $H_{a}$ for $D_{1}, D_{3}; $ Do not reject $H_{0}$ for $D_{2}$
* f
  * $q (.90; 6, 114) = 3.71 < T = 2.623$
  * Since We are only concerned with 3 machines specifically, and Tukey’s procedure considers all pairwise comparisons, using Bonferroni is more efficient. 

$\hat a$ 

MSE = 0.03097
