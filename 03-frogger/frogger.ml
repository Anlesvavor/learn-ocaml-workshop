open Base
open Scaffold

(* When you start the exercise, the compiler will complain that Frog.create,
 * World.create and create_frog are unused. You can remove this attribute once
 * you get going. *)
(* [@@@warning "-32"] *)


let () = Random.full_init[|1;25;6|]

let max_x = 9;;
let max_y = 11;;

module Orientation = struct
  type t =
    | Up
    | Right
    | Down
    | Left
end

module Faces = struct
  type t = (Orientation.t * Image.t) list
end

module Frog = struct
  type t =
    { position : Position.t
    ; orientation : Orientation.t
    ; faces: Faces.t
    ; alive: bool
    } [@@deriving fields]

  let create = Fields.create
end

module Kind = struct
  type t =
    | Unsafe
    | Safe
end

module Moving_Object = struct
  type t =
    { position: Position.t
    ; orientation: Orientation.t
    ; kind: Kind.t
    ; faces: Faces.t
    } [@@deriving fields]

  let create = Fields.create

  let move (obj : t) =
    let p = obj.position in
    let o = obj.orientation in
    let delta = match o with
    | Right -> if p.x < max_x then 1 else -max_x
    | Left -> if 0 < p.x then -1 else max_x
    | _ -> failwith "Not implemented"
    in
    let new_p = { p with x = p.x + delta } in
    { obj with position = new_p }

end

module World = struct
  type t =
    { frog  : Frog.t
    ; moving_objects : Moving_Object.t list
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  let x = Random.int max_x in
  let y = Random.int 1 in
  let position = Position.create ~x:x ~y:y in
  let faces =
    [(Orientation.Up, Image.Frog_up)
    ;(Orientation.Down, Image.Frog_down)
    ;(Orientation.Right, Image.Frog_right)
    ;(Orientation.Left, Image.Frog_left)
    ]
  in
  Frog.create ~position:position ~orientation:Up ~faces:faces ~alive:true
;;

let create_moving_object ?(x=None) ?(y=None) ?(kind=Kind.Safe) () =
  let x = match x with
    | Some x -> x
    | None -> Random.int_incl 1 max_x
  in
  let y = match y with
    | Some y -> y
    | None -> Random.int_incl 1 max_y
  in
  let position = Position.create ~x:x ~y:y in
  let faces = match kind with
    | Unsafe -> [(Orientation.Left, Image.Car1_left); (Orientation.Right, Image.Car1_right)]
    | Safe -> [(Orientation.Left, Image.Log1);(Orientation.Right, Image.Log2)]
  in
  Moving_Object.create
    ~position:position
    ~orientation:(if Random.bool () then Left else Right)
    ~kind
    ~faces
;;

let create_moving_hazard y = create_moving_object ~y ~kind:Unsafe ()
let create_moving_plarform x y = create_moving_object ~x ~y ~kind:Safe ()

let create () =
  let frog = create_frog () in
  (* let objects = *)
  (*   [create_moving_object () *)
  (*   ] *)
  (*   @ List.init 15 ~f:(fun _ -> create_moving_hazard ())in *)
  let objects =
    List.foldi Board.rows ~init:[] ~f:(fun i (acc : Moving_Object.t list) (row : Board.Row.t) ->
      let y = Some i in
      let x = Random.int_incl 1 max_x in
      acc @ match row with
      | Safe_strip -> []
      | Road -> List.init 3 ~f:(fun _ -> create_moving_hazard y )
      | River -> List.map [x;x+1;x+2] ~f:(fun x -> List.init 2 ~f:(fun _ -> create_moving_plarform (Some x) y )) |> List.join
    )
  in
  World.create ~frog:frog ~moving_objects:objects
;;

let draw_image orientation faces : Image.t =
  let face = List.find faces ~f:(fun (f_orientation, _) -> phys_equal orientation f_orientation) in
  let _, image = match face with
    | Some x -> x
    | None -> failwith "Face not found!"
  in
  image

let compute_colission (objects: Moving_Object.t list) (frog: Frog.t) =
  List.find objects ~f:(fun obj -> Position.equal obj.position frog.position)

(* let compute_colission_death (objects: Moving_Object.t list) (frog: Frog.t) : bool = *)
(*   List.exists objects ~f:(fun obj -> *)
(*     match obj.kind with *)
(*     | Kind.Unsafe -> Position.equal obj.position frog.position *)
(*     | Kind.Safe -> frog.hit *)
(*   ) *)

let compute_terrain (position : Position.t) =
  match List.nth Board.rows position.y with
  | Some x -> x
  | None -> failwith "Out of bounds"

(* let compute_terrain_death (position : Position.t) = *)
(*   match compute_terrain position with *)
(*   | Safe_strip | Road -> false *)
(*   | River -> true *)

let is_alive (frog: Frog.t) (world: World.t) =
  if not frog.alive then
    false
  else
  let terrain = compute_terrain frog.position in
  let colission = compute_colission world.moving_objects frog in
  match (terrain, colission) with
  | Safe_strip, _ -> true
  | River, Some ({ kind = Safe; _ }) -> true
  | River, None -> false
  | Road, Some  ({ kind = Unsafe; _ }) -> false
  | _, Some ({ kind = Safe; _ }) -> true
  | _, Some ({ kind = Unsafe; _ }) -> false
  | _, _ -> true


let tick (world : World.t) =
  let frog = world.frog in
  let objects = world.moving_objects in
  (* let hit = compute_colission objects frog in *)
  (* let hit = hit || compute_terrain_death world.frog.position in *)
  let alive = is_alive frog world in
  let frog = { frog with alive } in
  World.create ~frog ~moving_objects:(List.map objects ~f:Moving_Object.move)
  (* failwith *)
  (*   "This function will end up getting called every timestep, which happens to \ *)
  (*    be set to 1 second for this game in the scaffold (so you can easily see \ *)
  (*    what's going on). For the first step (just moving the frog/camel around), \ *)
  (*    you can just return [world] here. Later you'll want do interesting things \ *)
  (*    like move all the cars and logs, detect collisions and figure out if the \ *)
  (*    player has died or won. " *)
;;

let handle_input (world : World.t) (key : Key.t) =
  let frog_position = world.frog.position in
  let frog_orientation = world.frog.orientation in
  let delta_x, delta_y, orientation = match key with
  | Key.Arrow_down -> if frog_position.y > 0 then (0, -1, Orientation.Down) else (0, 0, frog_orientation)
  | Key.Arrow_up -> if frog_position.y < max_y then (0, 1, Orientation.Up) else (0, 0, frog_orientation)
  | Key.Arrow_right -> if frog_position.x < max_x then (1, 0, Orientation.Right) else (0, 0, frog_orientation)
  | Key.Arrow_left -> if frog_position.x > 0 then (-1, 0, Orientation.Left) else (0, 0, frog_orientation)
  in
  (* let hit = compute_colission world.moving_objects world.frog in *)
  (* let hit = hit || compute_terrain_death world.frog.position in *)
  let alive = is_alive world.frog world in
  let position = if alive
    then Position.create ~x:(frog_position.x + delta_x) ~y:(frog_position.y + delta_y)
    else frog_position
  in
  let frog = Frog.create
      ~position
      ~orientation
      ~alive
      ~faces:world.frog.faces
  in
  (*  *World.create ~frog ~moving_objects:world.moving_objects*)
  { world with frog }
  (*   This function will end up getting called whenever the player presses one of \ *)
  (*    the four arrow keys. What should the new state of the world be? Create and \ *)
  (*    return it based on the current state of the world (the [world] argument), \ *)
  (*    and the key that was pressed ([key]). Use either [World.create] or the \ *)
  (*    record update syntax: *)
  (*   { world with frog = Frog.create ... } *)
;;

let draw (world : World.t) =
  let frog_position = world.frog.position in
  let frog_image = match world.frog.alive with
    | true -> draw_image world.frog.orientation world.frog.faces
    | false -> Image.skull_and_crossbones
  in
  let objects_images = List.map world.moving_objects ~f:(fun o ->
      let image = draw_image o.orientation o.faces in
      (image, o.position)
    )
  in
  objects_images @ [(frog_image, frog_position)]
  (* failwith *)
  (*   "Return a list with a single item: a tuple consisting of one of the choices \ *)
  (*    in [Images.t] in [scaffold.mli]; and the current position of the [Frog]." *)
;;

let handle_event (world : World.t) (event : Event.t) =
  match event with
  | Event.Keypress key -> handle_input world key
  | Event.Tick -> tick world
  (* failwith *)
  (*   "This function should probably be just 3 lines long: [match event with ...]" *)
;;

let finished (_ : World.t) = false
  (* failwith *)
  (*   "This can probably just return [false] in the beginning." *)
;;
