module MongoCleanup.ProgressBar

open System
open System.Threading

type ProgressMessage =
    | Reset of int * string
    | Tick
    | Draw

let progressAgent : MailboxProcessor<ProgressMessage> = MailboxProcessor.Start(fun inbox ->
    let rec messageLoop total progress text newline = async {
            let! message = inbox.Receive()
            
            match message with
            | Reset(newTotal, newText) -> return! messageLoop (max newTotal 1) progress newText true
            | Tick -> return! messageLoop total (progress + 1) text newline
            | Draw ->
                if not Console.IsOutputRedirected then
                    let percentage = progress * 100 / total
                    if newline then
                        printf "\n%s %i/%i (%i%%)" text progress total percentage
                    else
                        Console.SetCursorPosition(0, Console.CursorTop)
                        printf "%s %i/%i (%i%%)" text progress total percentage

                return! messageLoop total progress text false
        }

    messageLoop 100 0 "" true
)

let public start (timeout : int) = MailboxProcessor.Start(fun _ ->
    let rec loop() = async {
       progressAgent.Post Draw
       Thread.Sleep(timeout)
       return! loop()
    }

    loop()
)

let public reset (total : int) (text : string) = progressAgent.Post <| Reset(total, text)

let public tick () = progressAgent.Post Tick
