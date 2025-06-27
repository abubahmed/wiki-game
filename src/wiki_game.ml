open! Core
module Article = String

module Wiki_network = struct
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]
end

module G = Graph.Imperative.Digraph.Concrete (Article)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let vertex_name v = v
    let edge_attributes __LOC_OF__ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_attributes v =
      [ `Shape `Box
      ; `Label (String.drop_suffix (String.drop_prefix v 1) 1)
      ; `Fillcolor 1000
      ]
    ;;

    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have =
   let keep_link link =the form "/wiki/<TITLE>". *)
let get_linked_articles contents =
  let keep_link link =
    let is_wiki = String.is_prefix link ~prefix:"/wiki" in
    match Wikipedia_namespace.namespace link, is_wiki with
    | None, true -> Some link
    | _ -> None
  in
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(R.attribute "href")
  |> List.filter_map ~f:keep_link
  |> List.dedup_and_sort ~compare:String.compare
;;

let get_article_title contents =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let get_contents (how_to_fetch : File_fetcher.How_to_fetch.t) destination =
  match how_to_fetch with
  | Remote ->
    File_fetcher.fetch_exn how_to_fetch ~resource:"en.wikipedia.org"
    ^ destination
  | Local _ -> File_fetcher.fetch_exn how_to_fetch ~resource:destination
;;

let bfs start_node max_depth how_to_fetch =
  let visited = String.Hash_set.create () in
  let to_visit = Queue.create () in
  let conns = Queue.create () in
  Queue.enqueue to_visit start_node;
  let rec traverse depth =
    if depth >= 0
    then (
      match Queue.dequeue to_visit with
      | None -> ()
      | Some current_node ->
        if not (Hash_set.mem visited current_node)
        then (
          Hash_set.add visited current_node;
          let contents = get_contents how_to_fetch current_node in
          let to_links = get_linked_articles contents in
          let from_title = get_article_title contents in
          List.iter
            ~f:(fun adj_node ->
              let to_title =
                get_article_title (get_contents how_to_fetch adj_node)
              in
              Queue.enqueue conns (from_title, to_title);
              Queue.enqueue to_visit adj_node;
              print_endline (from_title ^ " " ^ to_title ^ " " ^ Int.to_string depth);
              ())
            to_links);
        traverse (depth - 1))
  in
  traverse max_depth;
  Queue.to_list conns
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize
      ?(max_depth = 3)
      ~origin
      ~output_file
      ~(how_to_fetch : File_fetcher.How_to_fetch.t)
      ()
  : unit
  =
  let enclose_in_quotes s = String.of_char '"' ^ s ^ String.of_char '"' in
  let graph = G.create () in
  let conns = bfs ("/" ^ origin) max_depth how_to_fetch in
  List.iter conns ~f:(fun (from_article, to_article) ->
    G.add_edge
      graph
      (enclose_in_quotes from_article)
      (enclose_in_quotes to_article));
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 3 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
