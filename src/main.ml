(* dom and canvas *)
type canvas
type context
type document
external document: document = "document" [@@bs.val]
external getElementById : document -> string -> canvas = "" [@@bs.send]
external getContext : canvas -> string -> context = "" [@@bs.send]
external beginPath : context -> unit = "" [@@bs.send]
external moveTo : context -> float -> float -> unit = "" [@@bs.send]
external lineTo : context -> float -> float -> unit = "" [@@bs.send]
external stroke : context -> unit = "" [@@bs.send]
external pi : float = "Math.PI" [@@bs.val]

(* main code from here *)
type 'a tree = Leaf of 'a | Node of 'a * ('a tree) * ('a tree)

type line = {
    x : float;
    y : float;
    angle : float;
    length : float;
    width: float;
}

type fractal_parameters = {
    left_angle : float;
    right_angle : float;
    shrink_factor : float;
}


let endpoint line =
    ((line.x +. line.length *. (cos line.angle)), (-.(-.line.y +. line.length *. sin line.angle)))

let create_branches (p: fractal_parameters) (line: line) =
    let (x, y) = endpoint line in
    let left =
        { x = x; 
          y = y; 
          angle = pi *. (line.angle /. pi +. p.left_angle);
          length = line.length *. p.shrink_factor;
          width = line.width *. p.shrink_factor;
        }
    in
    let right =
        { x = x; 
          y = y; 
          angle = pi *. (line.angle /. pi -. p.left_angle);
          length = line.length *. p.shrink_factor;
          width = line.width *. p.shrink_factor;
        }
    in
    (left, right)

let rec create_tree depth p line =
    if depth < 0 
    then Leaf line
    else
        let (leftLine, rightLine) = create_branches p line in
        let left = create_tree (depth -1) p leftLine in
        let right = create_tree (depth -1) p rightLine in
        Node (line, left, right)


let draw_line context (line: line) =
    let (x, y) = endpoint line in
    beginPath context;
    moveTo context line.x line.y;
    lineTo context x y;
    stroke context

let rec draw_tree context = function
    | Leaf line -> draw_line context line
    | Node (line, left, right) ->
        draw_line context line;
        draw_tree context left;
        draw_tree context right

let () =
    let canvas = getElementById document "canvas" in
    let context = getContext canvas "2d" in
    let trunk = { x = 300.0; y = 600.0 ; angle = pi /. 2.0; length = 100.0; width = 4.0 } in
    let p = { left_angle = 0.1; right_angle =0.1; shrink_factor = 0.8 } in
    let tree = create_tree 10 p trunk in
    draw_tree context tree