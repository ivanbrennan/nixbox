self: super: files: hself: hsuper:
let
  extend = lhs: rhs: lhs // rhs lhs;
in
  super.lib.foldl extend hsuper (map (hol: hol self super hself) (map import files))
