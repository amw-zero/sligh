type point = {
  x: int;
  y: int;
}
type image_ast =
 | Segment of point * point
 | Point of point
 | Duplicate of image_ast
 | Distance of image_ast * image_ast