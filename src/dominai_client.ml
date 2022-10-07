open Core
open Import

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = flag "-url" (required string) ~doc:""
  and log_traffic = flag "-log-traffic" no_arg ~doc:""
  and strategy = flag "-strategy" (required string) ~doc:"" in
  fun () ->
    let (module Ai) =
      (* Map your strategy's name to its module *)
      match strategy with
      | "big-money" -> (module Dominai_ai.Big_money : Ai_intf.S)
      | _ -> failwith "Unknown strategy type"
    in
    let (module Client) = (module Client.Make (Ai) : Client.S) in
    Lwt_main.run (Client.main ~url ~log_traffic)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
