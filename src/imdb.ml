open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
    let keep_link link = 
    let is_wiki = String.is_prefix link ~prefix:"/wiki" in
    match Wikipedia_namespace.namespace link, is_wiki with
      | None, true -> Some link
      | _ -> None
  in
  ignore keep_link;
  let open Soup in
  parse contents
  $$ "a.ipc-primary-image-list-card__title"
  |> to_list
  |> List.map ~f:(fun ul -> texts ul |> String.concat ~sep:"" |> String.strip)
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given an IMDB page for an actor, print out a list of their main credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group ~summary:"imdb commands" [ "print-credits", print_credits_command ]
;;
