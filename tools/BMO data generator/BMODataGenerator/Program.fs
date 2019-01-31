open System.IO
open BMODataGenerator.Types
    
let startWritingServer (fileWriter : StreamWriter) totalNumUsers = MailboxProcessor<Msg>.Start(fun inbox ->
        let rec messageLoop numUsers (doneSocket : AsyncReplyChannel<unit> option) = async {
            if numUsers % 10000 = 0 then
                let percent = numUsers * 100 / totalNumUsers
                printfn "Has written %i%% (%i / %i) of users data" percent numUsers totalNumUsers
                
            if numUsers = totalNumUsers then
                match doneSocket with
                | Some s -> s.Reply ()
                | None -> printfn "Didn't have a done socket! The app is likely going to hang!"
            else
                let timeout = 500 // 0.5s
                let! msg = inbox.TryReceive(timeout = timeout)
                
                match msg with
                | Some (StopCallback socket) ->
                    printfn "Reply socket setup! Ready to rock!"
                    return! messageLoop numUsers (Some socket)
                    
                    
                | Some (Lines jsonLines) ->
                    jsonLines
                    |> List.iter fileWriter.WriteLine
                    
                    return! messageLoop (numUsers + 1) doneSocket
                    
                    
                | None ->
                    printfn "Didn't receive any message before timeout, but isn't done yet. Currently: %i/%i" numUsers totalNumUsers
                    return! messageLoop numUsers doneSocket
                    
        }
        
        messageLoop 0 None
    )

[<EntryPoint>]
let main argv =
    if argv.Length = 2 then
        let numUsers = argv.[0] |> int
        let filePath = argv.[1]
        use writer = System.IO.File.AppendText(filePath)
        use writingProcessor = startWritingServer writer numUsers
        let replyChannel = writingProcessor.PostAndAsyncReply(fun replyPid -> StopCallback replyPid)
        
        let processingResult = 
            BMODataGenerator.Generator.produceRowsForUsers numUsers writingProcessor
            |> Async.Parallel
            |> Async.RunSynchronously
         
        replyChannel
        |> Async.RunSynchronously
        
        printfn "Program terminating"
        0
    else
        printfn "Incorrect usage of BMO data generator."
        printfn "Expecting two arguments arguments indicating the number of users to generate data for and where to store the data."
        printfn "For example to generate data for 100 users:"
        printfn "  ./program 100 /my/path"
        1
