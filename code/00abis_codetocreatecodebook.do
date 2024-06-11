use "C:\Users\rivera30\OneDrive - NYU Langone Health\Dissertation\aim1\cannabispol_var\data\2_intermediate\APIS\cannabispol_2012_2021.dta" , clear
tab rml year
tab year regagency
tab  regagency productspermitted
tab  regagency year
tab  productspermitted year
tab   year productspermitted
tab productspermitted

tab productspermitted
egen productspermitted2 =group( productspermitted)
sort productspermitted2
tab   year productspermitted2
tab  productspermitted2 year
tab  productspermitted2 year
egen retailsales_onpremises2 =group( retailsales_onpremises )
tab  retailsales_onpremises year
tab  retailsales_onpremises2 year
tab  retailsales_offpremises year
egen retailsales_offpremises2 =group( retailsales_offpremises )
tab  retailsales_offpremises2 year
tab  pricingcontrols year
tab  pricingcontrols
tab  vertprohibited year
tab  industrymakeup year
tab  trackingreq year
tab  pesticides year
egen pesticides2=group(pesticides)
tab  pesticides2 year

tab  warningreq year
egen warningreq2=group( warningreq)
tab  warningreq2 year
sort warningreq2
tab  packagingreq year
egen packagingreq2 =group( packagingreq )
tab  packagingreq2 year
sort packagingreq2
tab  advertising_youth year
egen advertising_youth2 =group( advertising_youth )
tab  advertising_gen year
egen advertising_gen2 =group( advertising_gen )

tab  publicuse year
egen publicuse2 =group( publicuse )

tab  homedelivery year
egen homedelivery2 =group( homedelivery )

tab  opencontainer year
egen opencontainer2 =group( opencontainer )