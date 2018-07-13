PEW RESEARCH CENTER FOR THE PEOPLE & THE PRESS
POLITICAL TYPOLOGY 2017
Phase A: June 8-18 N=2,504
Phase B: June 27-July 9, 2017 N=2,505
Total N=5,009


***************************************************************************************************************************

This dataset includes cell phone interviews conducted using an RDD sample of cell phone numbers. 
Cell phone interviews include households that are cell-only as well as those that also have a landline phone. 
The dataset contains several weight variables. 

WEIGHT is the weight for the combined sample of all landline and cell phone interviews. 
Data for all Pew Research Center reports are analyzed using this weight.

***************************************************************************************************************************

Beginning in the Fall of 2008, the Pew Research Center started using respondents’ self-reported zip code as  
the basis for geographic variables such as region, state and county. We do this because the error rate in the original 
geographic information associated with the sample is quite high, especially for respondents from the cell phone sample. 

For respondents who do not provide a zip code or for those we cannot match, we use the sample geographic information. 
We continue to include the original sample geographic variables in the datasets (these variables are preceded by an ‘s’) 
for archival purposes only.

To protect the privacy of respondents, telephone numbers, county of residence and zip code have been removed from the data file.

***************************************************************************************************************************

Releases from this survey:

June 20, 2017. "Public Has Criticisms of Both Parties, but Democrats Lead on Empathy for Middle Class"
http://www.people-press.org/2017/06/20/public-has-criticisms-of-both-parties-but-democrats-lead-on-empathy-for-middle-class/

June 26, 2017. "Support for Same-Sex Marriage Grows, Even Among Groups That Had Been Skeptical"
http://www.people-press.org/2017/06/26/support-for-same-sex-marriage-grows-even-among-groups-that-had-been-skeptical/

July 10, 2017. "Sharp Partisan Divisions in Views of National Institutions"
http://www.people-press.org/2017/07/10/sharp-partisan-divisions-in-views-of-national-institutions/

July 20, 2017. "Since Trump’s Election, Increased Attention to Politics – Especially Among Women"
http://www.people-press.org/2017/07/20/since-trumps-election-increased-attention-to-politics-especially-among-women/

August 4, 2017. "Partisan Shifts in Views of the Nation, but Overall Opinions Remain Negative"
http://www.people-press.org/2017/08/04/partisan-shifts-in-views-of-the-nation-but-overall-opinions-remain-negative/

October 5, 2017. "The Partisan Divide on Political Values Grows Even Wider"
http://www.people-press.org/2017/10/05/the-partisan-divide-on-political-values-grows-even-wider/

October 24, 2017. "Political Typology Reveals Deep Fissures on the Right and Left"
http://www.people-press.org/2017/10/24/political-typology-reveals-deep-fissures-on-the-right-and-left/

March 1, 2018. "The Generation Gap in American Politics"
http://www.people-press.org/2018/03/01/the-generation-gap-in-american-politics/



Blog posts from this survey:

June 23, 2017. "Public support for ‘single payer’ increases, driven by Democrats"
http://www.pewresearch.org/fact-tank/2017/06/23/public-support-for-single-payer-health-coverage-grows-driven-by-democrats/

June 30, 2017. "Most Americans say the U.S. is among the greatest countries in the world"
http://www.pewresearch.org/fact-tank/2017/06/30/most-americans-say-the-u-s-is-among-the-greatest-countries-in-the-world/

July 3, 2017. "On abortion, persistent divides between – and within – the two parties"
http://www.pewresearch.org/fact-tank/2017/07/07/on-abortion-persistent-divides-between-and-within-the-two-parties-2/


***************************************************************************************************************************

*********

***THE 2017 POLITICAL TYPOLOGY*** 


The 2017 Political Typology was created using latent class analysis, and the following syntax shows the basic steps used to construct it.

We created syntax to incorporate survey weights to the latent class analysis through iterative resampling of the data. 
This iterative process will produce slightly different results given different (pseudo) random number generating algorithms and seeds.

**NOTE: If you are interested in fully replicating the typology groups from the report using this dataset, please contact us for additional syntax, which we can provide.
The full model was estimated in R 3.4.0 and made use of the poLCA (for latent class analysis) and mcclust packages (for combining the results from the iteratively resampled data).
The code given below will approximate the results of the full model but the results will not be exactly the same. When this "by hand" syntax is used, it correctly classifies 93% of cases. 

**IF YOU ARE SEEKING TO RECREATE THE TYPOLOGY USING A DIFFERENT DATASET: 
  The most straightforward way to replicate the same groups in this typology in a new data set is to use the code below. 
(It would also be possible to use the full code (which we can provide) with the iterative results to recreate the full original model and apply it to a new dataset, 
but it would be considerably more involved).



**TYPOLOGY CONSTRUCTION***.
**WE CREATE A GROUP OF UNENGAGED PEOPLE (BYSTANDERS), WHICH WE EXCLUDE FROM OUR TYPOLOGY CONSTRUCTION. THE DEFINITION HAS SHIFTED SLIGHTLY FROM YEAR TO YEAR
BUT IT IS LIKELY WE WILL CONTINUE TO USE THIS DEFINITION*.
****CREATE BYSTANDER GROUP*****.
**DEFINITION OF BYSTANDERS - NOT REGISTERED TO VOTE, SELDOM OR NEVER VOTE AND DO NOT FOLLOW GOVERNMENT AND PUBLIC AFFAIRS MOST OF THE TIME**. 
compute bys=0.
if ((reg>2) and (oftvote=4 or oftvote=5) and (q40>1)) bys=1.
format bys (F1.0).


**items used in creating the typology**.
compute regulate = q25b.
compute pooreasy = q25c.
compute workhard = q25k.
compute econfair = q51ll.
compute busprofit = q25n.
compute lawscost = q50r.
compute immburden = q25g.
compute allies = q50cc.
compute active = q50ee.
compute homosex = q50u.
compute blackeq = q50hh.
compute womenobs = q51nn.

***recode items so median category = 1 and all else = 0.
recode regulate (1=1)(2=0)(3,5,9=0).
recode pooreasy (1=0)(2=1)(3,5,9=0).
recode workhard (1=1)(2=0)(3,5,9=0).
recode econfair (1=1)(2=0)(3,5,9=0).
recode busprofit (1=1)(2=0)(3,5,9=0).
recode lawscost (1=0)(2=1)(3,5,9=0).
recode immburden (1=1)(2=0)(3,5,9=0).
recode allies (1=1)(2=0)(3,5,9=0).
recode active (1=1)(2=0)(3,5,9=0).
recode homosex (1=1)(2=0)(3,5,9=0).
recode blackeq (1=0)(2=1)(3,5,9=0).
recode womenobs (1=0)(2=1)(3,5,9=0).
recode partysum (2,9=0) into rep.
recode partysum (1,9=0) into dem.

*****Calculate the (logged) probability of class membership conditional on answers to typology items
For each item and each class, sum up the logged probabilities for each item and the logged class membership probability.
*****logged probability of class membership: 1 .
compute regulate_1=0.
if regulate=0 regulate_1=-3.338308.
if regulate=1 regulate_1=-0.03614229.
compute pooreasy_1=0.
if pooreasy=0 pooreasy_1=-3.758162.
if pooreasy=1 pooreasy_1=-0.02360294.
compute workhard_1=0.
if workhard=0 workhard_1=-0.2816153.
if workhard=1 workhard_1=-1.404719.
compute econfair_1=0.
if econfair=0 econfair_1=-4.495761.
if econfair=1 econfair_1=-0.01121888.
compute busprofit_1=0.
if busprofit=0 busprofit_1=-1.600352.
if busprofit=1 busprofit_1=-0.2254281.
compute lawscost_1=0.
if lawscost=0 lawscost_1=-9.21034.
if lawscost=1 lawscost_1=-0.000100005.
compute immburden_1=0.
if immburden=0 immburden_1=-4.947746.
if immburden=1 immburden_1=-0.007124712.
compute allies_1=0.
if allies=0 allies_1=-3.459669.
if allies=1 allies_1=-0.03194501.
compute active_1=0.
if active=0 active_1=-2.288486.
if active=1 active_1=-0.1069394.
compute homosex_1=0.
if homosex=0 homosex_1=-4.696432.
if homosex=1 homosex_1=-0.009169703.
compute blackeq_1=0.
if blackeq=0 blackeq_1=-3.802614.
if blackeq=1 blackeq_1=-0.02256506.
compute womenobs_1=0.
if womenobs=0 womenobs_1=-3.715603.
if womenobs=1 womenobs_1=-0.0246419.
compute rep_1=0.
if rep=0 rep_1=-0.004065046.
if rep=1 rep_1=-5.507362.
compute dem_1=0.
if dem=0 dem_1=-4.495761.
if dem=1 dem_1=-0.01121888.
compute lprob1 = sum(regulate_1 to dem_1) +-1.728883.
exe.
delete variables regulate_1 to dem_1.

*****logged probability of class membership: 2 .
compute regulate_2=0.
if regulate=0 regulate_2=-1.55391.
if regulate=1 regulate_2=-0.2375211.
compute pooreasy_2=0.
if pooreasy=0 pooreasy_2=-0.8607626.
if pooreasy=1 pooreasy_2=-0.5496349.
compute workhard_2=0.
if workhard=0 workhard_2=-1.424035.
if workhard=1 workhard_2=-0.275412.
compute econfair_2=0.
if econfair=0 econfair_2=-1.150881.
if econfair=1 econfair_2=-0.3803209.
compute busprofit_2=0.
if busprofit=0 busprofit_2=-0.7308875.
if busprofit=1 busprofit_2=-0.6567795.
compute lawscost_2=0.
if lawscost=0 lawscost_2=-1.746503.
if lawscost=1 lawscost_2=-0.1916239.
compute immburden_2=0.
if immburden=0 immburden_2=-4.171306.
if immburden=1 immburden_2=-0.01555241.
compute allies_2=0.
if allies=0 allies_2=-2.712691.
if allies=1 allies_2=-0.06866224.
compute active_2=0.
if active=0 active_2=-1.583542.
if active=1 active_2=-0.2297238.
compute homosex_2=0.
if homosex=0 homosex_2=-2.503599.
if homosex=1 homosex_2=-0.08532929.
compute blackeq_2=0.
if blackeq=0 blackeq_2=-1.103253.
if blackeq=1 blackeq_2=-0.403153.
compute womenobs_2=0.
if womenobs=0 womenobs_2=-0.9093703.
if womenobs=1 womenobs_2=-0.515466.
compute rep_2=0.
if rep=0 rep_2=-0.156726.
if rep=1 rep_2=-1.930596.
compute dem_2=0.
if dem=0 dem_2=-1.62186.
if dem=1 dem_2=-0.2200619.
compute lprob2 = sum(regulate_2 to dem_2) +-2.00566.
exe.
delete variables regulate_2 to dem_2.

*****logged probability of class membership: 3 .
compute regulate_3=0.
if regulate=0 regulate_3=-0.5964298.
if regulate=1 regulate_3=-0.8002303.
compute pooreasy_3=0.
if pooreasy=0 pooreasy_3=-1.44477.
if pooreasy=1 pooreasy_3=-0.2689262.
compute workhard_3=0.
if workhard=0 workhard_3=-0.5387506.
if workhard=1 workhard_3=-0.875813.
compute econfair_3=0.
if econfair=0 econfair_3=-5.266138.
if econfair=1 econfair_3=-0.005176888.
compute busprofit_3=0.
if busprofit=0 busprofit_3=-2.930764.
if busprofit=1 busprofit_3=-0.05483248.
compute lawscost_3=0.
if lawscost=0 lawscost_3=-1.252763.
if lawscost=1 lawscost_3=-0.3364722.
compute immburden_3=0.
if immburden=0 immburden_3=-1.730022.
if immburden=1 immburden_3=-0.19514.
compute allies_3=0.
if allies=0 allies_3=-0.9894723.
if allies=1 allies_3=-0.4648534.
compute active_3=0.
if active=0 active_3=-0.4089134.
if active=1 active_3=-1.091751.
compute homosex_3=0.
if homosex=0 homosex_3=-1.497216.
if homosex=1 homosex_3=-0.2532834.
compute blackeq_3=0.
if blackeq=0 blackeq_3=-2.47293.
if blackeq=1 blackeq_3=-0.08810727.
compute womenobs_3=0.
if womenobs=0 womenobs_3=-1.810874.
if womenobs=1 womenobs_3=-0.1785421.
compute rep_3=0.
if rep=0 rep_3=-0.02439145.
if rep=1 rep_3=-3.725693.
compute dem_3=0.
if dem=0 dem_3=-1.769631.
if dem=1 dem_3=-0.1868066.
compute lprob3 = sum(regulate_3 to dem_3) +-1.919575.
exe.
delete variables regulate_3 to dem_3.

*****logged probability of class membership: 4 .
compute regulate_4=0.
if regulate=0 regulate_4=-0.4620355.
if regulate=1 regulate_4=-0.9942523.
compute pooreasy_4=0.
if pooreasy=0 pooreasy_4=-1.913927.
if pooreasy=1 pooreasy_4=-0.1595821.
compute workhard_4=0.
if workhard=0 workhard_4=-0.6348783.
if workhard=1 workhard_4=-0.7550226.
compute econfair_4=0.
if econfair=0 econfair_4=-0.8209806.
if econfair=1 econfair_4=-0.5798185.
compute busprofit_4=0.
if busprofit=0 busprofit_4=-0.6881596.
if busprofit=1 busprofit_4=-0.6981597.
compute lawscost_4=0.
if lawscost=0 lawscost_4=-0.6539265.
if lawscost=1 lawscost_4=-0.7339692.
compute immburden_4=0.
if immburden=0 immburden_4=-0.6396064.
if immburden=1 immburden_4=-0.7497175.
compute allies_4=0.
if allies=0 allies_4=-0.3147107.
if allies=1 allies_4=-1.309333.
compute active_4=0.
if active=0 active_4=-0.2711528.
if active=1 active_4=-1.437588.
compute homosex_4=0.
if homosex=0 homosex_4=-0.6782586.
if homosex=1 homosex_4=-0.7082608.
compute blackeq_4=0.
if blackeq=0 blackeq_4=-2.120264.
if blackeq=1 blackeq_4=-0.1278334.
compute womenobs_4=0.
if womenobs=0 womenobs_4=-1.021651.
if womenobs=1 womenobs_4=-0.4462871.
compute rep_4=0.
if rep=0 rep_4=-0.3147107.
if rep=1 rep_4=-1.309333.
compute dem_4=0.
if dem=0 dem_4=-0.9100602.
if dem=1 dem_4=-0.515001.
compute lprob4 = sum(regulate_4 to dem_4) +-2.316432.
exe.
delete variables regulate_4 to dem_4.

*****logged probability of class membership: 5 .
compute regulate_5=0.
if regulate=0 regulate_5=-0.8066291.
if regulate=1 regulate_5=-0.5912409.
compute pooreasy_5=0.
if pooreasy=0 pooreasy_5=-0.2537805.
if pooreasy=1 pooreasy_5=-1.495494.
compute workhard_5=0.
if workhard=0 workhard_5=-2.365847.
if workhard=1 workhard_5=-0.0985722.
compute econfair_5=0.
if econfair=0 econfair_5=-0.2055784.
if econfair=1 econfair_5=-1.682957.
compute busprofit_5=0.
if busprofit=0 busprofit_5=-0.266203.
if busprofit=1 busprofit_5=-1.453647.
compute lawscost_5=0.
if lawscost=0 lawscost_5=-0.9010813.
if lawscost=1 lawscost_5=-0.5210953.
compute immburden_5=0.
if immburden=0 immburden_5=-1.22723.
if immburden=1 immburden_5=-0.3468709.
compute allies_5=0.
if allies=0 allies_5=-0.86404.
if allies=1 allies_5=-0.5472406.
compute active_5=0.
if active=0 active_5=-0.7203333.
if active=1 active_5=-0.6666806.
compute homosex_5=0.
if homosex=0 homosex_5=-1.037312.
if homosex=1 homosex_5=-0.4375847.
compute blackeq_5=0.
if blackeq=0 blackeq_5=-0.557224.
if blackeq=1 blackeq_5=-0.8504958.
compute womenobs_5=0.
if womenobs=0 womenobs_5=-0.3227734.
if womenobs=1 womenobs_5=-1.287854.
compute rep_5=0.
if rep=0 rep_5=-1.214242.
if rep=1 rep_5=-0.3523057.
compute dem_5=0.
if dem=0 dem_5=-0.2008836.
if dem=1 dem_5=-1.703791.
compute lprob5 = sum(regulate_5 to dem_5) +-2.163139.
exe.
delete variables regulate_5 to dem_5.

*****logged probability of class membership: 6 .
compute regulate_6=0.
if regulate=0 regulate_6=-0.5426934.
if regulate=1 regulate_6=-0.870316.
compute pooreasy_6=0.
if pooreasy=0 pooreasy_6=-0.1489367.
if pooreasy=1 pooreasy_6=-1.977778.
compute workhard_6=0.
if workhard=0 workhard_6=-1.091259.
if workhard=1 workhard_6=-0.409162.
compute econfair_6=0.
if econfair=0 econfair_6=-2.711747.
if econfair=1 econfair_6=-0.06872933.
compute busprofit_6=0.
if busprofit=0 busprofit_6=-2.200921.
if busprofit=1 busprofit_6=-0.1173219.
compute lawscost_6=0.
if lawscost=0 lawscost_6=-0.7858777.
if lawscost=1 lawscost_6=-0.6082906.
compute immburden_6=0.
if immburden=0 immburden_6=-0.347231.
if immburden=1 immburden_6=-1.226362.
compute allies_6=0.
if allies=0 allies_6=-0.6712485.
if allies=1 allies_6=-0.7155362.
compute active_6=0.
if active=0 active_6=-0.2791088.
if active=1 active_6=-1.412464.
compute homosex_6=0.
if homosex=0 homosex_6=-0.9339738.
if homosex=1 homosex_6=-0.4992083.
compute blackeq_6=0.
if blackeq=0 blackeq_6=-0.3790639.
if blackeq=1 blackeq_6=-1.153602.
compute womenobs_6=0.
if womenobs=0 womenobs_6=-0.5684183.
if womenobs=1 womenobs_6=-0.8356805.
compute rep_6=0.
if rep=0 rep_6=-1.382611.
if rep=1 rep_6=-0.2889128.
compute dem_6=0.
if dem=0 dem_6=-0.1298481.
if dem=1 dem_6=-2.105611.
compute lprob6 = sum(regulate_6 to dem_6) +-2.069287.
exe.
delete variables regulate_6 to dem_6.

*****logged probability of class membership: 7 .
compute regulate_7=0.
if regulate=0 regulate_7=-0.1695225.
if regulate=1 regulate_7=-1.858334.
compute pooreasy_7=0.
if pooreasy=0 pooreasy_7=-0.2576297.
if pooreasy=1 pooreasy_7=-1.482283.
compute workhard_7=0.
if workhard=0 workhard_7=-0.986495.
if workhard=1 workhard_7=-0.4666195.
compute econfair_7=0.
if econfair=0 econfair_7=-0.5749876.
if econfair=1 econfair_7=-0.827163.
compute busprofit_7=0.
if busprofit=0 busprofit_7=-0.5453118.
if busprofit=1 busprofit_7=-0.8666938.
compute lawscost_7=0.
if lawscost=0 lawscost_7=-0.1735466.
if lawscost=1 lawscost_7=-1.836828.
compute immburden_7=0.
if immburden=0 immburden_7=-0.000100005.
if immburden=1 immburden_7=-9.21034.
compute allies_7=0.
if allies=0 allies_7=-0.0997267.
if allies=1 allies_7=-2.354771.
compute active_7=0.
if active=0 active_7=-0.2105118.
if active=1 active_7=-1.661624.
compute homosex_7=0.
if homosex=0 homosex_7=-0.1575463.
if homosex=1 homosex_7=-1.925775.
compute blackeq_7=0.
if blackeq=0 blackeq_7=-0.2888127.
if blackeq=1 blackeq_7=-1.38291.
compute womenobs_7=0.
if womenobs=0 womenobs_7=-0.5630114.
if womenobs=1 womenobs_7=-0.8427883.
compute rep_7=0.
if rep=0 rep_7=-2.796604.
if rep=1 rep_7=-0.06295785.
compute dem_7=0.
if dem=0 dem_7=-0.003395589.
if dem=1 dem_7=-5.686975.
compute lprob7 = sum(regulate_7 to dem_7) +-2.741507.
exe.
delete variables regulate_7 to dem_7.

*****logged probability of class membership: 8 .
compute regulate_8=0.
if regulate=0 regulate_8=-0.04676871.
if regulate=1 regulate_8=-3.085834.
compute pooreasy_8=0.
if pooreasy=0 pooreasy_8=-0.02511698.
if pooreasy=1 pooreasy_8=-3.696743.
compute workhard_8=0.
if workhard=0 workhard_8=-2.769981.
if workhard=1 workhard_8=-0.0647126.
compute econfair_8=0.
if econfair=0 econfair_8=-0.2163131.
if econfair=1 econfair_8=-1.637236.
compute busprofit_8=0.
if busprofit=0 busprofit_8=-0.09010183.
if busprofit=1 busprofit_8=-2.451527.
compute lawscost_8=0.
if lawscost=0 lawscost_8=-0.006548812.
if lawscost=1 lawscost_8=-5.031744.
compute immburden_8=0.
if immburden=0 immburden_8=-0.514313.
if immburden=1 immburden_8=-0.9110824.
compute allies_8=0.
if allies=0 allies_8=-0.3664201.
if allies=1 allies_8=-1.181597.
compute active_8=0.
if active=0 active_8=-0.7690644.
if active=1 active_8=-0.622589.
compute homosex_8=0.
if homosex=0 homosex_8=-0.7222883.
if homosex=1 homosex_8=-0.6648313.
compute blackeq_8=0.
if blackeq=0 blackeq_8=-0.1383921.
if blackeq=1 blackeq_8=-2.046062.
compute womenobs_8=0.
if womenobs=0 womenobs_8=-0.09296307.
if womenobs=1 womenobs_8=-2.421674.
compute rep_8=0.
if rep=0 rep_8=-3.55014.
if rep=1 rep_8=-0.02914113.
compute dem_8=0.
if dem=0 dem_8=-0.000100005.
if dem=1 dem_8=-9.21034.
compute lprob8 = sum(regulate_8 to dem_8) +-1.988218.
exe.
delete variables regulate_8 to dem_8.

***********Assign respondents to the class that with the highest probability of membership.
compute typosCalcByHand = $sysmis.
if (lprob1 >= lprob2)&(lprob1 >= lprob3)&(lprob1 >= lprob4)&(lprob1 >= lprob5)&(lprob1 >= lprob6)&(lprob1 >= lprob7)&(lprob1 >= lprob8) typosCalcByHand = 1 .
if (lprob2 >= lprob1)&(lprob2 >= lprob3)&(lprob2 >= lprob4)&(lprob2 >= lprob5)&(lprob2 >= lprob6)&(lprob2 >= lprob7)&(lprob2 >= lprob8) typosCalcByHand = 2 .
if (lprob3 >= lprob1)&(lprob3 >= lprob2)&(lprob3 >= lprob4)&(lprob3 >= lprob5)&(lprob3 >= lprob6)&(lprob3 >= lprob7)&(lprob3 >= lprob8) typosCalcByHand = 3 .
if (lprob4 >= lprob1)&(lprob4 >= lprob2)&(lprob4 >= lprob3)&(lprob4 >= lprob5)&(lprob4 >= lprob6)&(lprob4 >= lprob7)&(lprob4 >= lprob8) typosCalcByHand = 4 .
if (lprob5 >= lprob1)&(lprob5 >= lprob2)&(lprob5 >= lprob3)&(lprob5 >= lprob4)&(lprob5 >= lprob6)&(lprob5 >= lprob7)&(lprob5 >= lprob8) typosCalcByHand = 5 .
if (lprob6 >= lprob1)&(lprob6 >= lprob2)&(lprob6 >= lprob3)&(lprob6 >= lprob4)&(lprob6 >= lprob5)&(lprob6 >= lprob7)&(lprob6 >= lprob8) typosCalcByHand = 6 .
if (lprob7 >= lprob1)&(lprob7 >= lprob2)&(lprob7 >= lprob3)&(lprob7 >= lprob4)&(lprob7 >= lprob5)&(lprob7 >= lprob6)&(lprob7 >= lprob8) typosCalcByHand = 7 .
if (lprob8 >= lprob1)&(lprob8 >= lprob2)&(lprob8 >= lprob3)&(lprob8 >= lprob4)&(lprob8 >= lprob5)&(lprob8 >= lprob6)&(lprob8 >= lprob7) typosCalcByHand = 8 .
fre typosCalcByHand.


********
***IDEOLOGICAL CONSISTENCY SCALE**
***these are the ideological consistency scale items and the syntax used to create the scale, based on Phase A only***

compute GovWaste2 = q25a.
compute Regulate2 = q25b.
compute PoorEasy2 = q25c.
compute GovNeedy2 = q25d.
compute Discrimi2 = q25f.
compute ImmBurdn2 = q25g.
compute PeaceStr2 = q25i.
compute BusProft2 = q25n.
compute LawsCost2 = q50r.
compute Homosex2 = q50u.

recode GovWaste2 PoorEasy2 PeaceStr2 LawsCost2 (1=1) (2=-1) (else=0).
recode Regulate2 GovNeedy2 Discrimi2 ImmBurdn2 BusProft2 Homosex2 (2=1) (1=-1) (else=0).
val lab GovWaste2 to Homosex2 -1"Liberal response"  0"Both/DK" 1"Conservative response".

count consCount10 = GovWaste2 Regulate2 PoorEasy2 GovNeedy2 Discrimi2 ImmBurdn2 PeaceStr2 BusProft2 LawsCost2 Homosex2(1).
count libCount10 = GovWaste2 Regulate2 PoorEasy2 GovNeedy2 Discrimi2 ImmBurdn2 PeaceStr2 BusProft2 LawsCost2 Homosex2(-1).
compute ideoconsist = consCount10 - libCount10.

