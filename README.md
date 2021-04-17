[[https://www.kaggle.com/mrklees/modeling-smokers-in-the-nhs]{.ul}](https://www.kaggle.com/mrklees/modeling-smokers-in-the-nhs)

**smokers.csv**

Fumeurs par âge en pourcentage de la population

  -----------------------------------------------------
  Year          1974 - 2014   numerical
  ------------- ------------- -------------------------
  Method        Weighted\     categorical
                Unweighted    

  Sex           Male\         categorical
                Female        

  16 and over   17 - 51       numerical (pourcentage)

  16 - 24       20 - 47       numerical (pourcentage)

  25 - 34       20 - 55       numerical (pourcentage)

  35 - 49       20 - 55       numerical (pourcentage)

  50 - 59       18 - 53       numerical (pourcentage)

  60 and over   10 - 44       numerical (pourcentage)
  -----------------------------------------------------

**prescriptions.csv**

Prescriptions en milliers attribués au tabac

  Year                                 2004/05 ... 2014/15   caractère
  ------------------------------------ --------------------- ----------------------
  All pharmacotherapy                  1348 - 2564           numerical (milliers)
  Nicotine Replacement Therapy (NRT)   766 - 2076            numerical (milliers)
  Bupropion (Zyban)                    21 - 136              numerical (milliers)
  Varenicline (Champix)                22 - 987              numerical (milliers)
  Net Ingredient Cost of All           38.1k - 65.9k         numerical (milliers)
  Net Ingredient Cost of NRT           18.2k - 43.5k         numerical (milliers)
  Net Ingredient Cost of Bupropion     807 - 5151            numerical (milliers)
  Net Ingredient Cost of Varenicline   760 - 33.5k           numerical (milliers)

**metrics.csv**

Dépenses par ménage liées au tabac

  Year                                                   1980 - 2015           
  ------------------------------------------------------ --------------------- --
  Tobacco Price Index                                    100 - 1294            
  Retail Prices Index                                    100 - 387             
  Tobacco Price Index relative to Retail Prices Index    100 - 335             
  Real Households' Disposable Income                     98,7 - 196            
  Affordability of Tobacco Index                         58,7 - 104            
  Household Expenditure on Tobacco                       7 006 - 19 400        
  Household Expenditure Total                            214 000 - 1 150 000   
  Expenditure on Tobacco as Percentage of Exependiture   1,7 - 3,3             

**fatalities.csv**

Morts liées au tabac

*ICD-10 is the 10th revision of the International Statistical
Classification of Diseases and Related Health Problems (ICD)*

+-----------------+------------------------------------+-------------+
| Year            | 2004 - 2014                        |             |
+=================+====================================+=============+
| ICD10 Code      | C00-D48 ....                       | categorical |
+-----------------+------------------------------------+-------------+
| ICD10 Diagnosis | All deaths\                        | categorical |
|                 | All cancers                        |             |
|                 |                                    |             |
|                 | Cervical\                          |             |
|                 | Bladder                            |             |
|                 |                                    |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+
| Diagnosis Type  | All deaths\                        | categorical |
|                 | All cancers                        |             |
|                 |                                    |             |
|                 | Digestive diseases which can be    |             |
|                 | caused by smoking                  |             |
|                 |                                    |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+
| Metric          | Number of observed deaths\         | categorical |
|                 | Attributable number                |             |
+-----------------+------------------------------------+-------------+
| Sex             | Male\                              | categorical |
|                 | Female                             |             |
+-----------------+------------------------------------+-------------+
| Value           | 100\                               | caractère   |
|                 | 241683\                            |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+

**admissions.csv**

Admissions à l\'hôpital attribuées au tabac

+-----------------+------------------------------------+-------------+
| Year            | 2004 - 2014                        |             |
+=================+====================================+=============+
| ICD10 Code      | C00-D48 ....                       | categorical |
+-----------------+------------------------------------+-------------+
| ICD10 Diagnosis | All admissions\                    | categorical |
|                 | All cancers                        |             |
|                 |                                    |             |
|                 | Cervical\                          |             |
|                 | Bladder                            |             |
|                 |                                    |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+
| Diagnosis Type  | All admissions\                    | categorical |
|                 | All cancers                        |             |
|                 |                                    |             |
|                 | Digestive diseases which can be    |             |
|                 | caused by smoking                  |             |
|                 |                                    |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+
| Metric          | Number of admissions\              | categorical |
|                 | Attributable number                |             |
+-----------------+------------------------------------+-------------+
| Sex             | Male\                              | categorical |
|                 | Female                             |             |
+-----------------+------------------------------------+-------------+
| Value           | 400\                               | caractère   |
|                 | 11011882\                          |             |
|                 | \...                               |             |
+-----------------+------------------------------------+-------------+
