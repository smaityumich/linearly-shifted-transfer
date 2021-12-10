

clean.data <- function(year.mortality = 10, path = '~/projects/posterior-drift/UKBB/')
{
    
  ######################################
  ######### data load ##################
  
  print("data loading ... ")
  
  bd <- read.table(paste(path, "mortality.tab", sep = ''), header=TRUE, sep="\t")
  
  print("data loaded.")
  print("data cleaning ...")
  
  ######################################
  ##### data clean and data frame ######
  
  
  
  
  ####### date formatting ##############
  
  bd$f.53.0.0 <- as.Date(bd$f.53.0.0)
  bd$f.53.1.0 <- as.Date(bd$f.53.1.0)
  bd$f.53.2.0 <- as.Date(bd$f.53.2.0)
  bd$f.53.3.0 <- as.Date(bd$f.53.3.0)
  bd$f.40000.0.0 <- as.Date(bd$f.40000.0.0)
  bd$f.40000.1.0 <- as.Date(bd$f.40000.1.0)
  bd$f.40000.0.0[is.na(bd$f.40000.0.0)] = bd$f.40000.1.0[is.na(bd$f.40000.0.0)]
  
  
  
  ## only keep data from first visit and first measurement: code "0.0"
  #### - except for genetic PC's. UDI: 22009-0.0 to 22009-0.40
  
  cov.udi = colnames(bd)[stringr::str_detect(colnames(bd), "\\.0\\.0")]
  cov.udi = cov.udi[!stringr::str_detect(cov.udi, "22009")]
  a = colnames(bd)[stringr::str_detect(colnames(bd), "22009")]
  cov.udi = c(cov.udi, a)
  
  
  ## get column names from ukb44126.html files 
  
  infos.cov = XML::readHTMLTable(paste(path, 'ukb44126.html', sep = ''))[[2]]
  fields = stringr::str_split(infos.cov$UDI, '-')
  fields.vec = rep('', length(fields))
  for(i in 1:length(fields))
  {
    fields.vec[i] = paste('f.', fields[[i]][1], '.', fields[[i]][2], sep = '')
  }
  
  infos.cov$UDI = fields.vec
  
  
  rows.infos.cov = infos.cov$UDI %in% cov.udi
  infos.cov = infos.cov[rows.infos.cov, ]
  
  inds = stringr::str_detect(infos.cov$UDI, "22009")
  infos.cov$Description[inds] = stringr::str_replace(infos.cov$UDI[inds], "f\\.22009\\.0", "PC")
  infos.cov$Type[inds] = "Continuous"
  
  colnames.cov = stringr::str_split(infos.cov$Description, 'Uses')
  colnames.cov.vec = rep('', length(colnames.cov))
  for(i in 1:length(colnames.cov))
    colnames.cov.vec[i] = colnames.cov[[i]][1]
  
  infos.cov$Description = colnames.cov.vec
  
  
  ## remove other columns and set the column names
  
  bd = bd[, infos.cov$UDI]
  colnames(bd) = infos.cov$Description 
  
  
  ###############################################
  ###############################################
  ###############################################
  
  
  ##### death time after first visit (in year)
  
  ########## if death didn't occur set year to 2000
  ########## mortality = 1 iff death_time < 20
  
  bd$`Date of attending assessment centre` = strptime(bd$`Date of attending assessment centre`, format="%Y-%m-%d")
  bd$`Date of death` = strptime(bd$`Date of death`, format="%Y-%m-%d")
  bd[['Death year after 1st visit']] = as.numeric(difftime(bd$`Date of death`, bd$`Date of attending assessment centre`,
                                                           units = 'days'))/365
  
  bd$`Death year after 1st visit`[is.na(bd$`Death year after 1st visit`)] = 2000
  bd[['mortality']] = as.numeric(bd$`Death year after 1st visit` < year.mortality)
  
  
  drops = c('Date of attending assessment centre', 'Death year after 1st visit', 'Date of death')
  bd = bd[, !(names(bd) %in% drops)]
  
  
  
  ###############################################
  ###############################################
  
  
  ### - only keep observations that was used in calculation
  ###   of genetic PC
  
  bd = bd[!is.na(bd$`Used in genetic principal components`), ]
  
  
  ###############################################
  ###############################################
  
  
  #### education qualification < 0 implies missing entries; remove them
  bd = bd[bd$Qualifications > 0, ]
  bd$Qualifications = as.factor(bd$Qualifications)
  
  
  
  # Family history of prostate, breast, bowel, lung cancer (coding 13, 5, 4, 3)
  coding = c(13, 5, 4, 3)
  temp = rep(0, nrow(bd))
  temp[bd$`Illnesses of father` %in% coding] = 1
  temp[bd$`Illnesses of mother` %in% coding] = 1
  temp[bd$`Illnesses of siblings` %in%coding] = 1
  bd[['Family history of cancer']] = temp
  
  
  drops = c('Illnesses of father', 'Illnesses of mother', 'Illnesses of siblings',
            'Used in genetic principal components', 'Age at death', 'Non-cancer illness code, self-reported')
  bd = bd[, !(names(bd) %in% drops)]
  
  
  
  
  
  
  
  #### ethnic background
  bd = bd[bd$`Ethnic background` >0, ]
  temp = bd$`Ethnic background` 
  ethnicities = c('White', 'Mixed', 'Asian or Asian British', 'Black or Black British', 
                  'Chinese', 'Other')
  temp.eth = rep('', length(temp))
  for(i in 1:length(temp))
  {
    eth = temp[i]
    if(is.na(eth))
      temp.eth[i] = NA
    else{
      if(eth > 10)
        eth = eth %% 1000
      temp.eth[i] = ethnicities[eth]
    }
    
  }
  
  bd$`Ethnic background` = temp.eth
  
  
  
  
  
  ##################################################
  ##################################################
  
  
  
  
  ### - The NA's in beef/pork/fish/veg intake and gluten sensitivity
  ### are replaced by 0
  
  replace_na <- function(x, val = 0)
  {
    x[is.na(x)] = 0
    return(x)
  }
  
  
  bd$`Beef intake` = replace_na(bd$`Beef intake`, 0)
  bd$`Pork intake` = replace_na(bd$`Pork intake`, 0)
  bd$`Fish consumer` = replace_na(bd$`Fish consumer`, 0)
  bd$`Vegetable consumers` = replace_na(bd$`Vegetable consumers`, 0)
  bd$`Diagnosed with coeliac disease or gluten sensitivity` = replace_na(bd$`Diagnosed with coeliac disease or gluten sensitivity`, 0)
  
  
  
  ###########################################################
  
  ### removing entries with values (-1: don't know, -3: prefer not to answer)
  ### for the question medication for cholesterol/bp/diabetes/hormones 
  
  
  bd = bd[!bd$`Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones` %in% c(-1, -3), ]
  bd$`Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones` = replace_na(bd$`Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones`, -7)
  bd$`Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones` = as.factor(bd$`Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones`)
  
  
  
  ### cleaning milk type, cheese, cereal, salt intake
  
  bd = bd[bd$`Milk type used` >= 0, ]
  bd$`Milk type used` = as.factor(bd$`Milk type used`)
  bd = bd[bd$`Cheese intake` >= 0, ]
  bd = bd[bd$`Cereal intake` >= 0, ]
  bd = bd[bd$`Salt added to food` >=0, ]
  bd = bd[bd$`Alcohol intake frequency.` >= 0, ]
  
  
  
  ### vitamin supplement cleaning
  bd = bd[bd$`Vitamin and mineral supplements` != -3, ]
  bd$`Vitamin and mineral supplements` = as.factor(bd$`Vitamin and mineral supplements`)
  
  
  
  ### repalcing NA's in diagnosis as none
  
  a = as.character(bd$`Diagnoses - main ICD10`)
  b = rep('', length(a))
  for(i in 1:length(a))
  {
    char = a[i]
    if(is.na(char))
      b[i] = "none"
    else{
      if(char >= 'C00' && char <= 'D48')
        b[i] = 'Neoplasms'
      else if(char >= 'I05' && char <= 'I89')
        b[i] = 'Circulatory System'
      else if(char >= 'J09' && char <= 'J99')
        b[i] = 'Respiratory System'
      else if(char >= 'K20' && char <= 'K93')
        b[i] = 'Digestive System'
      else if(char >= 'V01' && char <= 'Y84')
        b[i] = 'external causes of mortality and morbidity'
      else
        b[i] = "none"
    }
  }
  
  
  bd$`Diagnoses - main ICD10` = as.factor(b)
  
  
  
  #### doctors recommendation
  
  bd$`Doctor restricts physical activity due to heart condition` = replace_na(bd$`Doctor restricts physical activity due to heart condition`, 0)
  a[a == 2] = NA
  a = bd$`Doctor restricts physical activity due to heart condition`
  
  
  
  ##### cleaning treatment code: if not NA then 1; else 0
  inds = is.na(bd$`Treatment/medication code`)
  bd$`Treatment/medication code`[inds] = 0
  bd$`Treatment/medication code`[!inds] = 1
  
  print("data frame ready.")
  
  ##### return
  return(bd)
  
}






