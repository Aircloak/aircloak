module BMODataGenerator.Generator

open BMODataGenerator
open Types
open FSharp.Json

module Data =
    let merchantNames =
       [|
            "407 ETR";
            "407 ETR";
            "407 ETR";
            "7-ELEVEN";
            "BELL, VIRIGIN MOBILE, THE SOURCE";
            "CHEVRON";
            "CHEVRON";
            "CHEVRON";
            "ESSO";
            "GROCERY GATEWAY";
            "LONDON DRUGS";
            "LONDON DRUGS";
            "STAPLES";
            "STAPLES";
            "STAPLES";
            "STAPLES";
            "THE BRICK";
            "TICKETMASTER";
            "URBAN BARN";
            "VISIONS ELECTRONICS"
        |]
     
    let merchantCities =
         [|
             "Barrie";
             "Bellevie";
             "Brampton";
             "Cambridge";
             "Ottawa";
             "Ottawa";
             "Ottawa";
             "Ottawa";
             "St. Thomas";
             "Welland"
          |]
     
    let commonTransactionAmounts =
        [|
            "1000.0"
            "100.0"
            "99.0"
            "99.0"
            "0.99"
            "1.99"
            "1.99"
            "1.99"
            "2.99"
            "2.99"
            "2.99"
            "2.99"
            "9.99"
            "9.99"
            "9.99"
            "9.99"
        |]
        
    let spouses =
        [|
            "HELEN"
            "MARTHA"
            "CHRISTINE"
            "ROBIN STRADINGE"
            "601-12-1596"
            "391-30-1913"
            "HERS:SEE MM01"
            "APP-N/COAPP-Y"
            "JEAN J*SHAW"
            "ALICE"
            "FRED"
        |]
       
module RandomData =
    let pick (rnd : System.Random) (choices : 'a []) =
        let numChoices = choices.Length
        let pick = rnd.Next(0, numChoices - 1)
        choices.[pick]
        
    let merchantName rnd = Data.merchantNames |> pick rnd
    
    let merchantSic (rnd : System.Random) = rnd.Next(0, 1000) |> sprintf "%i"
    
    let merchantCity rnd = Data.merchantCities |> pick rnd
    
    let merchantState _rnd = "Ontario"
    
    let tranAmt (rnd : System.Random) =
        [|
            rnd.NextDouble()
            rnd.NextDouble()
            rnd.NextDouble()
        |]
        |> Array.map (sprintf "%f")
        |> Array.append Data.commonTransactionAmounts
        |> pick rnd
    
    let tranDt (rnd : System.Random) =
        sprintf "2019-01-%i" <| rnd.Next(1, 31)
        
    let spouse rnd = Data.spouses |> pick rnd
    
    let workplace rnd = Data.merchantNames |> pick rnd
    
    let annualInc (rnd : System.Random) = sprintf "%i.0" <| (rnd.Next (14, 60)) * 1000
    
    let cardNbr (rnd : System.Random) =
        let rec rndNr () = seq {
            yield! [rnd.Next(1, 9)]
            yield! rndNr ()
        }
        rndNr ()
        |> Seq.take 16
        |> Seq.toList
        |> List.fold (fun acc i -> sprintf "%s%i" acc i) ""
            
    let genesisId (rnd : System.Random) = rnd.Next(67108933, 67908933)
    
let produceRow rnd userId spouse workplace annualInc cardNbr =
    {
        userId = userId
        merchName = RandomData.merchantName rnd
        merchSic = RandomData.merchantSic rnd
        merchCity = RandomData.merchantCity rnd
        merchState = RandomData.merchantState rnd
        tranAmt = RandomData.tranAmt rnd
        tranDt = RandomData.tranDt rnd
        spouse = spouse
        workPlace = workplace
        annualInc = annualInc
        cardNbr = cardNbr
        genesisId = RandomData.genesisId rnd
    }
    
let produceRowsForUser jsonConfig currentUserId (pid : MailboxProcessor<Msg>) =
    async {
        let rnd = System.Random(currentUserId)
        let spouse = RandomData.spouse rnd
        let workplace = RandomData.workplace rnd
        let annualInc = RandomData.annualInc rnd
        let cardNbr = RandomData.cardNbr rnd
        let numRecordsToProduce = rnd.Next(5, 50)
        let lines =
            List.fold (fun acc _n ->
                let row =
                    produceRow rnd currentUserId spouse workplace annualInc cardNbr
                    |> Json.serializeEx jsonConfig
                (row :: acc)
              ) [] [1 .. numRecordsToProduce]
        pid.Post (Lines lines)
        return ()
    }
    
let produceRowsForUsers numUsers (pid : MailboxProcessor<Msg>) =
    let jsonConfig = JsonConfig.create(jsonFieldNaming = Json.snakeCase, unformatted = true)
    let rec lineGeneratorAsync currentUserId = seq {
        yield! [produceRowsForUser jsonConfig currentUserId pid]
        if currentUserId < numUsers then
            yield! lineGeneratorAsync (currentUserId + 1)
    }
    lineGeneratorAsync 1