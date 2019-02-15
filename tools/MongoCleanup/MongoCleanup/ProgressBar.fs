module MongoCleanup.ProgressBar

open System
open System.Threading

type ProgressMessage =
    | Reset of int * string
    | Tick of int
    | Draw
    | Finish

let progressAgent : MailboxProcessor<ProgressMessage> = MailboxProcessor.Start(fun inbox ->
    let draw total progress text =
        if not Console.IsOutputRedirected && total > 0 then
            let percentage = progress * 100 / total
            let pertwenty = progress * 20 / total
            let bar = sprintf "[%s%s]" (String.replicate pertwenty "#") (String.replicate (20 - pertwenty) " ")

            Console.SetCursorPosition(0, Console.CursorTop)
            printf "%s %s %i/%i (%i%%)" bar text progress total percentage

    let rec messageLoop total progress text = async {
            let! message = inbox.Receive()

            match message with
            | Reset(newTotal, newText) ->
                draw total total text
                printfn ""
                return! messageLoop newTotal 0 newText
            | Tick(newProgress) -> return! messageLoop total (progress + newProgress) text
            | Draw ->
                draw total progress text
                return! messageLoop total progress text
            | Finish ->
                draw total total text
                printfn ""
                return! messageLoop total total text
        }

    messageLoop 0 0 ""
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

let public tick progress = progressAgent.Post <| Tick(progress)

let public finish() = progressAgent.Post <| Finish
