### question 1
* a
  * $ \bar{y}_{1.} = (34 + 23 + 36)÷3 = 31 $
  * $ \bar{y}_{2.} = (40 + 29 + 42)÷3 = 37 $
* b
  * $ \hat{α_{1}} = \bar{y}_{1.} - \bar{y}_{..} = 31 - 34 = -3 $
  * $ \hat{α_{1}} = \bar{y}_{2.} - \bar{y}_{..} = 37 - 34 = 3 $
* c
  * Since those differences are measured over the same level of factor A, it does not imply that there is an interaction. There should be sets of data from multiple levels of both A and B to determine if interaction exists.
* d
  * ![picture 2](../images/32e7188973200db36e98b322cd4bc45a440be3ab06ceac517bafce124be4554f.png)

  * From the interaction plot, there are little interaction between factors A and B. The lines of both factor levels of A seem to be parrallel with each other.

### question 2
* a
  $ \bar{y}_{..} = 269 $
  * $ \hat{β_{1}} = \bar{y}_{.1} = \frac{(250+288)}{2} - 269 = 0 $
  * $ \hat{β_{2}} = \bar{y}_{.2} = \frac{(265 + 273)}{2} - 269 = 0 $
  * $ \hat{β_{3}} = \bar{y}_{.3} = \frac{(268 + 270)}{2} - 269 = 0 $
  * $ \hat{β_{4}} = \bar{y}_{.4} = \frac{(269 + 269)}{2} - 269 = 0 $
  * Since all levels of B's estimated main effect are 0, we would expect for B not having an effect on the response value as the level of A is held constant.
* b
  * ![picture 1](../images/38c8b557e0ebe782b2d1fceb3da07086815857e3e28e134045d74f7a872b41c6.png)
  * I can say there is an interaction between factor A and B. The interaction plot shows that mean effect lines of B is not constant over different levels of B. The differences between level of A decrease over level of B. This interaction is important because depending on the help of factor A, the effect of B can either increase or decrease over its level.
* c
  * ![picture 2](../images/75660fddf84448c0f967f2568e929abb5c7e9fc7af8a1d5f5c80a582d15e615b.png)
  * From the transformed interaction plot shows that a logarithmic transformation of the $ \bar{y} $ is not helpful in reducing the interactions. There is still a same interactive pattern.

### question 3
* a
  * ![picture 3](../images/17649672247fc14c001d40d717e1863d01ee2d09fb1a1eaab3787608c69da266.png)![picture 4](../images/21c324fb7ed5d534135abb5b21d6c73511acd0b592c8e8da71724451ff948ced.png)  
  ![picture 5](../images/183ad3b3b2dbeb741a3d40c00e6552bfc2545746d44b553c0c66d2965705e85b.png)
  * 
* b
  ![picture 6](../images/4476e7a19f1fb11c1170526dc7d92ac44e09a6e92168ab16fca6b402b67be56d.png)  ![picture 7](../images/b87744cae9ca73d1f7c202c80cae7bbbfc32cfba1c16943c2700a37b4076c42f.png)  

* c
  ![picture 8](../images/77f9ec28399271a1dfd6a68b5b939f67fd7b9548331d7007ba1ca3cda11f321b.png)  
  * It can study departures from the necessary assumptions of constancy and possible outliers for two-factor-ANOVA model. I can see there is no outlier or any pattern.
* d
  * ![picture 9](../images/89f45d18f028e88ccb36455a3ee463aee05410584812760a1b6165067f2a7b01.png)  
  * ![picture 10](../images/b05faa3c2f1de467fee37466bf3c76aec90648c88017c42fab21fa10594327dc.png)  
  * The correlation coefficient between the ordered residuals and their expected values under normality is .98772. 
  There are some slight deviations from normality, however, overall the normality assumption appears to be valid.
  

  

### question 4
* a
  * ![picture 1](../images/438847910e6e7109885e5626bc500dfb73c2d096b7bfed47d6ac0b0955b97c71.png)  
  * Yes, there are factor effects on factor A. The increase of factor A is depending on the level of factor B.
* b
  * ![picture 2](../images/69518242b619249dc08c2baf608e08224dbd4e70b3fdb349b6a1dec0b11f0667.png)  
  * By checking Type I, we can see source accounts the most of the variability. Yet, source b also has some effects as well.
* c
  * $ H0:αβ_{ij} = 0,  ∀ i,j$
  * $ Ha:αβ_{ij} ≠ 0,  $ At least one is not equal to 0.
  * The decision rule is α = 0.05 and since p-value is <0.0001, we reject $ H0 $ and conclude $ Ha $. Therefore, there is an interaction effect between factors A and B on hours of relief.
* d
  * $ H0:α_{i.} = 0 ∩ β_{.j} = 0,  ∀ i,j$
  * $ Ha:α_{i.} ≠ 0 ∩ β_{.j} ≠ 0,  $ At least one is not equal to 0.
  * * The decision rule is $ F(.95, 2, 27) =3.354, α=0.05 $ and since p-value is <0.0001. Hence, we reject $ H0 $ and conclude $ Ha $. Therefore, there is a main effect of either or both of factors A and B on hours of relief. 
* e
  * Kimball inequality is $ α ≦ 1 - 0.95^3 = 0.1426 $. Therefore, the upper bound of family level of significant is  $ .1426 $.
* f
  * Yes, the results in both part (c) and (d) confirm the graphic analysis in part (a).

### question 5

$ α=6, β=6, n=10 $, 5 contrasts of factor A level means and 4 contrasts of factor B level means.
$ αβ_{ij} = 0, ∀ i,j, α=0.05$

1. For Scheffe Procedure, $ S^2 = (a-1)F[1-α; a-1, (n-1)ab] = 5*F[0.95; 5, 324]=11.209 ∴ S=3.348 $
2. For Bonferroni Procedure, $ B = t[1-α/2g;(n-1)ab] = t[1-\frac{0.05}{2*9}; 324]= ±3.015 $
3. For Combined Factor A and Factor B Family, $ S^2 = (a+b-2)F[1-α; a+b-2, (n-1)ab] = 10*F[0.95; 10, 324]= 9.299 ∴ S=3.049 $

Therefore, procedure(2); Bonferroni Procedure will be most efficient.

### question 6
* a
  * F-statistic = 5.57. p-value = 0.0048. ∴ There is a difference among the factor variables.
  &nbsp;
  ![picture 3](../images/6f094461eac923eaaaca971c08cf44dd403f154043c2a1b48022b40e69f33605.png) 

  &nbsp;
  * There is a difference between the mean effect of ovens 3 and 4, 2 and 4. Meanwhile, there is no difference in mean effect of two levels of ovens.
  * There is a difference between the mean effect of cooling temps 5 and 15. Otheres are not different from with each other.
  &nbsp;
  ![picture 1](../../images/f9aeb1f51b0f744b9cd6e9badfd4ad6e33fcdf576f1d04d88acad9c02afd02a5.png)  ![picture 2](../../images/ac60a7b5cf26fe0e0a6499de6b4ceb21f3ef1ef24b4db208c4a62ee00cb82ebf.png)  
&nbsp;

* b
  * A set of three contrasts are:
  $ \begin{matrix} 
    -1 & 1 & 0 & 0 \\ 
    0 & 0 & -1 & 1 \\ 
    1 & 1 & -1 & -1 \\ 
    \end{matrix}$
* c
  * We can see there is no difference between the cooling temp 5℃ and 10℃, 15℃ and 20℃. However, there is a difference between the temp less than 12.5℃ and those greater than 12.5℃. Hence, we conclude that there is a jump in tile strength around 12.5℃, but otherwise there is no affect of cooling temperature on line strength.
  ![picture 3](../../images/f5a557297c8374cdb77e44d2e2ed4a1abd9d96d822efc75679283315d18693de.png)

### question 7
* $ H0:θ = 0, no interaction present $
* $ Ha:θ ≠ 0,  $ interaction $ θ_{α_{i}β_{j}} $ present.
* We reject H0 if $ F^*  > F(0.975, 1, 2) = 38.506$
* p-value: 0.5228 is larger than 0.025. Therefore, we fail to reject the null hypothesis. There is not enough evidence to conclude that there are some interactions between the location and the week on the time.
![picture 4](../../images/902e77f615df0be1e9b9cee43da5519778bddf2db9b150e60920e3b16776785a.png)  



