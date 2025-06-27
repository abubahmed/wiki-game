open! Core
module City = String
module Road = String

module Highway_system = struct
  module Connection = struct
    module T = struct
      type t = City.t * Road.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let get_line s =
      let s_cleaned =
        String.tr
          ~target:'.'
          ~replacement:'a'
          (String.tr ~target:' ' ~replacement:'_' s)
      in
      match String.split s_cleaned ~on:',' with
      | [] -> None
      | head :: tail -> Some (head, tail)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Connection.get_line s with
        | Some (head, x) ->
          (* Friendships are mutual; a connection between a and b means we should also
             consider the connection between b and a. *)
          (List.filter ~f:(fun (outer_left, _, outer_right) ->
             if String.equal outer_left outer_right then false else true))
            (List.concat_map
               ~f:(fun elem ->
                 List.map
                   ~f:(fun inner_elem ->
                     ( City.of_string elem
                     , Road.of_string head
                     , City.of_string inner_elem ))
                   x)
               x)
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let results = Highway_system.of_file input_file in
        printf !"%{sexp: Highway_system.t}\n" results]
;;

module My_string = struct
  include String

  let default = ""
end

module G = Graph.Imperative.Graph.ConcreteLabeled (City) (My_string)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let vertex_name v = v
    let edge_attributes e = [ `Dir `None; `Label (snd3 e) ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let system = Highway_system.of_file input_file in
        let graph = G.create () in
        Set.iter system ~f:(fun (city1, road, city2) ->
          G.add_edge_e graph (city1, road, city2));
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
