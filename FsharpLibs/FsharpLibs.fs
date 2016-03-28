namespace FsharpLibs

module Playground = 
    type ConfigArticleCode = 
        | ARTICLE_UNKNOWN = 0
        | ARTICLE_CONTRACT_EMPL_TERM = 101
        | ARTICLE_POSITION_EMPL_TERM = 110
        | ARTICLE_INCOME_GROSS = 501
        | ARTICLE_INCOME_NETTO = 502
        | ARTICLE_CONTRACT_STAT_TERM = 102
        | ARTICLE_CONTRACT_WORK_TERM = 103
        | ARTICLE_CONTRACT_TASK_TERM = 104
        | ARTICLE_SCHEDULE_WORK = 201
        | ARTICLE_SALARY_BASE = 202
        | ARTICLE_TIMESHEET_SCHEDULE = 251
        | ARTICLE_TIMESHEET_WORKING = 252
        | ARTICLE_TIMESHEET_ABSENCE = 253
        | ARTICLE_TIMEHOURS_WORKING = 254
        | ARTICLE_TIMEHOURS_ABSENCE = 255
        | ARTICLE_HEALTH_INCOME_SUBJECT = 301
        | ARTICLE_SOCIAL_INCOME_SUBJECT = 302
        | ARTICLE_GARANT_INCOME_SUBJECT = 303
        | ARTICLE_HEALTH_INCOME_PARTICIP = 305
        | ARTICLE_SOCIAL_INCOME_PARTICIP = 306
        | ARTICLE_GARANT_INCOME_PARTICIP = 307
        | ARTICLE_HEALTH_BASIS_GENERAL = 311
        | ARTICLE_HEALTH_BASIS_MANDATORY = 312
        | ARTICLE_HEALTH_BASIS_LEGALCAP = 313
        | ARTICLE_SOCIAL_BASIS_GENERAL = 321
        | ARTICLE_SOCIAL_BASIS_PENSION = 322
        | ARTICLE_SOCIAL_BASIS_LEGALCAP = 323
        | ARTICLE_GARANT_BASIS_PENSION = 331
        | ARTICLE_GARANT_BASIS_LEGALCAP = 332
        | ARTICLE_HEALTH_EMPLOYEE_GENERAL = 341
        | ARTICLE_HEALTH_EMPLOYEE_MANDATORY = 342
        | ARTICLE_SOCIAL_EMPLOYEE_GENERAL = 351
        | ARTICLE_SOCIAL_EMPLOYEE_PENSION = 352
        | ARTICLE_GARANT_EMPLOYEE_PENSION = 361
        | ARTICLE_HEALTH_EMPLOYER_GEENRAL = 371
        | ARTICLE_HEALTH_EMPLOYER_MANDATORY = 372
        | ARTICLE_SOCIAL_EMPLOYER_GENERAL = 373
        | ARTICLE_TAXING_INCOME_SUBJECT = 401
        | ARTICLE_TAXING_INCOME_HEALTH = 402
        | ARTICLE_TAXING_INCOME_SOCIAL = 403
        | ARTICLE_TAXING_ADVANCES_INCOME = 411
        | ARTICLE_TAXING_ADVANCES_HEALTH = 412
        | ARTICLE_TAXING_ADVANCES_SOCIAL = 413
        | ARTICLE_TAXING_ADVANCES_BASIS_GENERAL = 414
        | ARTICLE_TAXING_ADVANCES_BASIS_SOLIDARY = 415
        | ARTICLE_TAXING_ADVANCES_GENERAL = 416
        | ARTICLE_TAXING_ADVANCES_SOLIDARY = 417
        | ARTICLE_TAXING_ADVANCES_TOTAL = 418
        | ARTICLE_TAXING_ALLOWANCE_PAYER = 421
        | ARTICLE_TAXING_ALLOWANCE_CHILD = 422
        | ARTICLE_TAXING_ALLOWANCE_DISABILITY = 423
        | ARTICLE_TAXING_ALLOWANCE_STUDYING = 424
        | ARTICLE_TAXING_REBATE_PAYER = 431
        | ARTICLE_TAXING_REBATE_CHILD = 432
        | ARTICLE_TAXING_BONUS_CHILD = 433
        | ARTICLE_TAXING_WITHHOLD_INCOME = 451
        | ARTICLE_TAXING_WITHHOLD_HEALTH = 452
        | ARTICLE_TAXING_WITHHOLD_SOCIAL = 453
        | ARTICLE_TAXING_WITHHOLD_BASIS_GENERAL = 454
        | ARTICLE_TAXING_WITHHOLD_GENERAL = 456

    type ProcessCategory = 
        | CATEGORY_TERMS  = 0
        | CATEGORY_START  = 1
        | CATEGORY_TIMES  = 2
        | CATEGORY_AMOUNT = 3
        | CATEGORY_GROSS  = 4
        | CATEGORY_NETTO  = 5
        | CATEGORY_FINAL  = 9

    type Article(code, catg, pendings) = 

        member x.Code with get() = code
        member x.Catg with get() = catg
        member x.Pendings with get() = pendings

        new () as this = Article(ConfigArticleCode.ARTICLE_UNKNOWN, ProcessCategory.CATEGORY_TERMS,  [| |]) then
            System.Console.WriteLine(this)

        member x.isEqualToArticle(other : Article) = 
            x.Code = other.Code

         override x.Equals(obj) =
            match obj with
            | :? Article as key -> (x.isEqualToArticle(key))
            | _ -> false
     
        override x.GetHashCode() = 
            hash [x.Code]

        member x.CompareToArticle(other:Article) =
            compare x.Code other.Code

        interface System.IComparable with
          member x.CompareTo obj =
              match obj with
              | :? Article as other -> 
                  x.CompareToArticle(other)
              | _ -> invalidArg "obj" "cannot compare values of different types"
       
        override x.ToString() = 
            x.Code.ToString()

    let EMPTY_PENDING_NAMES = [||]

    let PendingArticleNames1(code1:ConfigArticleCode) = 
        [| code1 |]

    let PendingArticleNames2(code1:ConfigArticleCode, code2:ConfigArticleCode) = 
        [| code1; code2 |]

    let PendingArticleNames3(code1:ConfigArticleCode, code2:ConfigArticleCode, code3:ConfigArticleCode) = 
        [| code1; code2; code3 |]

    let PendingArticleNames4(code1:ConfigArticleCode, code2:ConfigArticleCode, code3:ConfigArticleCode, code4:ConfigArticleCode) = 
        [| code1; code2; code3; code4 |]

    let PendingArticleNames5(code1:ConfigArticleCode, code2:ConfigArticleCode, code3:ConfigArticleCode, code4:ConfigArticleCode, code5:ConfigArticleCode) = 
        [| code1; code2; code3; code4; code5 |]

    let PendingArticleNames9(code1:ConfigArticleCode, code2:ConfigArticleCode, code3:ConfigArticleCode, code4:ConfigArticleCode, code5:ConfigArticleCode, code6:ConfigArticleCode, code7:ConfigArticleCode, code8:ConfigArticleCode, code9:ConfigArticleCode) = 
        [| code1; code2; code3; code4; code5; code6; code7; code8; code9 |]

    let ConfigureContractTermArticles() =
        let configCategory = ProcessCategory.CATEGORY_TERMS

        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_CONTRACT_EMPL_TERM, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_POSITION_EMPL_TERM, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_CONTRACT_EMPL_TERM
                )
            )
        |]
        articleArray

    let ConfigurePositionTimeArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_TIMES;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_SCHEDULE_WORK, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_POSITION_EMPL_TERM
                ));
            new Article(ConfigArticleCode.ARTICLE_TIMESHEET_SCHEDULE, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SCHEDULE_WORK
                ));
            new Article(ConfigArticleCode.ARTICLE_TIMESHEET_WORKING, configCategory,
                PendingArticleNames2(
                    ConfigArticleCode.ARTICLE_TIMESHEET_SCHEDULE,
                    ConfigArticleCode.ARTICLE_POSITION_EMPL_TERM
                ));
            new Article(ConfigArticleCode.ARTICLE_TIMESHEET_ABSENCE, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TIMESHEET_WORKING
                ));
            new Article(ConfigArticleCode.ARTICLE_TIMEHOURS_WORKING, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TIMESHEET_WORKING
                ));
            new Article(ConfigArticleCode.ARTICLE_TIMEHOURS_ABSENCE, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TIMESHEET_ABSENCE
                ))
        |]
        articleArray


    let ConfigureGrossIncomeArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_AMOUNT;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_SALARY_BASE, configCategory,
                PendingArticleNames2(
                    ConfigArticleCode.ARTICLE_TIMEHOURS_WORKING,
                    ConfigArticleCode.ARTICLE_TIMEHOURS_ABSENCE
                ))
        |]
        articleArray


    let ConfigureTotalIncomeArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_FINAL;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_INCOME_GROSS, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_INCOME_NETTO, configCategory,
                PendingArticleNames9(
                    ConfigArticleCode.ARTICLE_INCOME_GROSS,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_TOTAL,
                    ConfigArticleCode.ARTICLE_TAXING_BONUS_CHILD,
                    ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_GENERAL,
                    ConfigArticleCode.ARTICLE_HEALTH_EMPLOYEE_GENERAL,
                    ConfigArticleCode.ARTICLE_HEALTH_EMPLOYEE_MANDATORY,
                    ConfigArticleCode.ARTICLE_SOCIAL_EMPLOYEE_GENERAL,
                    ConfigArticleCode.ARTICLE_SOCIAL_EMPLOYEE_PENSION,
                    ConfigArticleCode.ARTICLE_GARANT_EMPLOYEE_PENSION
                ))
        |]
        articleArray


    let ConfigureNettoDeductsArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_TOTAL, configCategory,
                PendingArticleNames2(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_GENERAL,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_SOLIDARY
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_BASIS_GENERAL
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_SOLIDARY, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_BASIS_SOLIDARY
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_BASIS_GENERAL
                ));
            new Article(ConfigArticleCode.ARTICLE_HEALTH_EMPLOYEE_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_HEALTH_BASIS_GENERAL
                ));
            new Article(ConfigArticleCode.ARTICLE_HEALTH_EMPLOYEE_MANDATORY, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_HEALTH_BASIS_MANDATORY
                ));
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_EMPLOYEE_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_BASIS_GENERAL
                ));
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_EMPLOYEE_PENSION, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_BASIS_PENSION
                ));
            new Article(ConfigArticleCode.ARTICLE_GARANT_EMPLOYEE_PENSION, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_GARANT_BASIS_PENSION
                ))
        |]
        articleArray


    let ConfigureBasisHealthArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_HEALTH_INCOME_SUBJECT, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_HEALTH_INCOME_PARTICIP, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_HEALTH_INCOME_SUBJECT
                ));
            new Article(ConfigArticleCode.ARTICLE_HEALTH_BASIS_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_HEALTH_INCOME_PARTICIP
                ));
            new Article(ConfigArticleCode.ARTICLE_HEALTH_BASIS_MANDATORY, configCategory,
                PendingArticleNames2(
                    ConfigArticleCode.ARTICLE_HEALTH_BASIS_GENERAL,
                    ConfigArticleCode.ARTICLE_HEALTH_INCOME_PARTICIP
                ));
            new Article(ConfigArticleCode.ARTICLE_HEALTH_BASIS_LEGALCAP, configCategory,
                PendingArticleNames2(
                    ConfigArticleCode.ARTICLE_HEALTH_BASIS_GENERAL,
                    ConfigArticleCode.ARTICLE_HEALTH_INCOME_PARTICIP
                ))
        |]
        articleArray


    let ConfigureBasisSocialArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_INCOME_SUBJECT, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_INCOME_PARTICIP, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_INCOME_SUBJECT
                ));
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_BASIS_GENERAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_INCOME_PARTICIP
                ));
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_BASIS_PENSION, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_INCOME_PARTICIP
                ));
            new Article(ConfigArticleCode.ARTICLE_SOCIAL_BASIS_LEGALCAP, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_SOCIAL_INCOME_PARTICIP
                ))
        |]
        articleArray


    let ConfigureBasisGarantArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_GARANT_INCOME_SUBJECT, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_GARANT_INCOME_PARTICIP, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_GARANT_INCOME_SUBJECT
                ));
            new Article(ConfigArticleCode.ARTICLE_GARANT_BASIS_PENSION, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_GARANT_INCOME_PARTICIP
                ));
            new Article(ConfigArticleCode.ARTICLE_GARANT_BASIS_LEGALCAP, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_GARANT_INCOME_PARTICIP
                ))
        |]
        articleArray


    let ConfigureBasisTaxingArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_INCOME_SUBJECT, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_TAXING_INCOME_HEALTH, configCategory,
                EMPTY_PENDING_NAMES);
            new Article(ConfigArticleCode.ARTICLE_TAXING_INCOME_SOCIAL, configCategory,
                EMPTY_PENDING_NAMES)
        |]
        articleArray


    let ConfigureBasisAdvancesArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO;
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME, configCategory,
                PendingArticleNames1 (
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_SUBJECT
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_HEALTH, configCategory,
                PendingArticleNames1 (
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_HEALTH
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_SOCIAL, configCategory,
                PendingArticleNames1 (
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_SOCIAL
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_BASIS_GENERAL, configCategory,
                PendingArticleNames3 (
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_HEALTH,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_SOCIAL
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ADVANCES_BASIS_SOLIDARY, configCategory,
                PendingArticleNames1 (
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_BASIS_GENERAL
                ))
        |]
        articleArray


    let ConfigureBasisWithholdArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO; 
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_INCOME, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_SUBJECT
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_HEALTH, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_HEALTH
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_SOCIAL, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_INCOME_SOCIAL
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_BASIS_GENERAL, configCategory,
                PendingArticleNames3(
                    ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_INCOME,
                    ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_HEALTH,
                    ConfigArticleCode.ARTICLE_TAXING_WITHHOLD_SOCIAL
                ))
        |]
        articleArray


    let ConfigureAllowanceTaxisArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO; 
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_PAYER, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_DISABILITY, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_STUDYING, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_CHILD, configCategory,
                PendingArticleNames1(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_INCOME
                ))
        |]
        articleArray


    let ConfigureRebateTaxisArticles() =
        let configCategory : ProcessCategory = ProcessCategory.CATEGORY_NETTO; 
        
        let articleArray = [|
            new Article(ConfigArticleCode.ARTICLE_TAXING_REBATE_PAYER, configCategory,
                PendingArticleNames4(
                    ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_PAYER,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_TOTAL,
                    ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_DISABILITY,
                    ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_STUDYING
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_REBATE_CHILD, configCategory,
                PendingArticleNames3(
                    ConfigArticleCode.ARTICLE_TAXING_ALLOWANCE_CHILD,
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_TOTAL,
                    ConfigArticleCode.ARTICLE_TAXING_REBATE_PAYER
                ));
            new Article(ConfigArticleCode.ARTICLE_TAXING_BONUS_CHILD, configCategory,
                PendingArticleNames3(
                    ConfigArticleCode.ARTICLE_TAXING_ADVANCES_TOTAL,
                    ConfigArticleCode.ARTICLE_TAXING_REBATE_PAYER,
                    ConfigArticleCode.ARTICLE_TAXING_REBATE_CHILD
                ))
        |]
        articleArray

    let ConfigureArticles() =
        let articlesArray = [|
            ConfigureContractTermArticles()
            ConfigurePositionTimeArticles()
            ConfigureGrossIncomeArticles()
            ConfigureTotalIncomeArticles()
            ConfigureNettoDeductsArticles()
            ConfigureBasisHealthArticles()
            ConfigureBasisSocialArticles()
            ConfigureBasisGarantArticles()
            ConfigureBasisTaxingArticles()
            ConfigureBasisAdvancesArticles()
            ConfigureBasisWithholdArticles()
            ConfigureAllowanceTaxisArticles()
            ConfigureRebateTaxisArticles()
        |]
       
        Array.fold(fun acc elem -> Array.concat( Array.toSeq([| acc; elem |]))) [||] articlesArray
