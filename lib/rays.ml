module Pixel = struct
  type t = int

  let create (r, g, b) =
    (* rgb *)
    let r = (r land 255) lsl 16 in
    let g = (g land 255) lsl 8 in
    let b = b land 255 in
    r lor g lor b
end


module Image = struct
  type t = { pixels: Pixel.t array array}

end
