
/*

A linear bezier curve between two points p0 and p1 is given by:

LB(p0, p1, t) = (t - 1) p0 + t p1

This creates a straight line moving from p0 to p1 for t in [0, 1]

A quadratic bezier curve between 3 points p0, p1, and p2 is a curve defined by:

QB(t) = (t - 1) LB(p0, p1, t) + t * LB(p1, p2, t)

It might not be obvious at first, but this creates a smooth line connecting p0 and p2. 
Because the curve doesn't neccessarily pass through p2, we call p2 a control point which allows us to modify how the curve looks.

*/

/*

A bezier triangle is the extension of a bezier curve, but for surfaces.

We understand a linear bezier curve, so lets try to understand a linear bezier triangle, to do this we have three points p0, p1, p2, so if we consider the equation

LBT(p0, p1, p2, s, t, u) = s * p0 + t * p1 + u * p2 

where s + t + u = 1 and s, t, u in [0, 1]

Then we can see that it defines the flat triangle connecting all three points.

In order to extend this to a quadratic bezier triangles we need to add another "layer" of primitives (which in this case are triangles) as we did in the curve case. 

One idea might be, ok let's use two triangles. We can extend the first triangle by just adding one one vertex that shares two vertices with the previous:

QBT(p0, p1, p2, p3, s, t, u) = s * LBT(p0, p1, p2, s, t, u) + t * LBT(p1, p2, p3, s, t, u) + u * ?????

Ok so having two triangles actually isn't enough because we need another LBT term to put in place of ?????, This tells us that we need to have 3 triangles.

If we construct a triangle like this out of 3 triangles, and label them like:
   p0
   /\
p1/__\p2
 /\  /\
/__\/__\
p3 p4  p5

Then we can say that:

QBT(p0, p1, p2, p3, p4, p5, s, t, u) = s * LBT(p0, p1, p2, s, t, u) + t * LBT(p1, p3, p4, s, t, u) + u * LBT(p2, p4, p5, s, t, u)

In general you can then do cubic bezier triangles by repeating this process.

*/


/*

degree 1 (aka linear), has 1 + 2 = 3 points.

 /\
/__\

degree 2 (aka quadratic), has 1 + 2 + 3 = 6 points.

   /\
  /__\
 /\  /\
/__\/__\


degree 3 (aka cubic), has 1 + 2 + 3 + 4 = 10 points.

     /\
    /__\
   /\  /\
  /__\/__\
 /\  /\  /\
/__\/__\/__\

degree 4 

       /\
      /__\
     /\  /\
    /__\/__\
   /\  /\  /\
  /__\/__\/__\
 /\  /\  /\  /\
/__\/__\/__\/__\

degree n has 1 + 2 + 3 + ... + (n + 1) = (n + 2) * (n + 1) / 2

*/

Bezier_Triangle :: struct {
    degree: int;
    // the control points are stored by reading them like a book from top to bottom and left to right as seen in the previous comments
    control_points: [..] Vector3;
}

init_bezier_triangle :: (bt: *Bezier_Triangle, degree: int) {
    bt.degree = degree;
    count := (degree + 1) * (degree + 2) / 2;
    array_resize(*bt.control_points, count);
}

// for any row with value n, it has n + 1 entries, eg row 0 has 1 entry.
// the row is determined by how far we are from the top of the triangle.
control_point_flat_index :: (row: int, col: int) -> int {
    return row * (row + 1) / 2 + col;
}

/*
convert from barycentric indices (i, j, k) where i + j + k = n
to the flat array index used by control_point_flat_index.
barycentric indices are the integer version of barycentric coordinates:
they encode position within the triangle but sum to n instead of 1.
*/
barycentric_indices_to_flat_index :: (n: int, i: int, j: int, k: int) -> int {
    row := n - i;
    col := j;
    return control_point_flat_index(row, col);
}

get_control_point :: (bt: *Bezier_Triangle, i: int, j: int, k: int) -> Vector3 {
    idx := barycentric_indices_to_flat_index(bt.degree, i, j, k);
    return bt.control_points[idx];
}

set_control_point :: (bt: *Bezier_Triangle, i: int, j: int, k: int, value: Vector3) {
    idx := barycentric_indices_to_flat_index(bt.degree, i, j, k);
    bt.control_points[idx] = value;
}

/*

Suppose we have the procedure where for every vertex in a shape, we replace it with the following shape

   /\
  /__\

If we start the procedure on itself, we get the following

                                               /\
                                              /__\
                             /\              /\  /\
                            /__\            /__\/__\
              /\           /\  /\          /\  /\  /\
             /__\         /__\/__\        /__\/__\/__\
    /\      /\  /\       /\  /\  /\      /\  /\  /\  /\
   /__\    /__\/__\     /__\/__\/__\    /__\/__\/__\/__\

Usually this sequence of shapes is not viewed like this, but instead as adding layers, but stick with this new approach as it helps us in this algorithm.

Additionally we can see that for every vertex that got expanded, if it has a barycentric index of (a, b, c) it becomes the triangle with barycentric indices 

(a + 1, b, c), (a, b + 1, c), (a, b, c + 1)

These are valid barycentric indices because their sum adds to to the degree of the next level (because it just adds 1 to each sum).

---

This algorithm evaluates it in a similar way, we start with some degree n bezier triangle, and the goal is to reach degree 0 (a single point) by iteratively 
decreasing in degrees. Each time we decrease in degree we do so by using (s, t, u) to evaluate a position inside each triangle, the resulting points form a degree n - 1
bezier triangle which we can then repeat the process on.

*/

evaluate_bezier_triangle :: (bt: *Bezier_Triangle, s: float, t: float, u: float) -> Vector3 {
    n := bt.degree;
    num_control_points := (n + 1) * (n + 2) / 2;

    points_at_current_degree: [..] Vector3;
    array_resize(*points_at_current_degree, num_control_points);
    defer array_free(points_at_current_degree);

    for 0..num_control_points-1 points_at_current_degree[it] = bt.control_points[it];

    for level: 0..n-1 {
        degree_before_reduction := n - level;
        reduced_degree := degree_before_reduction - 1;
        reduced_count := (reduced_degree + 1) * (reduced_degree + 2) / 2;

        points_at_reduced_degree: [..] Vector3;
        array_resize(*points_at_reduced_degree, reduced_count);

        // iterates over all barycentric indices of the reduced triangle
        for i: 0..reduced_degree {
            for j: 0..reduced_degree - i {
                k := reduced_degree - i - j;

                // each vertex in the reduced triangle is expanded to the 3 bindices of the one before reduction and we do use s, t, u there to compute.
                corner_towards_i := points_at_current_degree[barycentric_indices_to_flat_index(degree_before_reduction, i + 1, j,     k)];
                corner_towards_j := points_at_current_degree[barycentric_indices_to_flat_index(degree_before_reduction, i,     j + 1, k)];
                corner_towards_k := points_at_current_degree[barycentric_indices_to_flat_index(degree_before_reduction, i,     j,     k + 1)];

                blended := s * corner_towards_i + t * corner_towards_j + u * corner_towards_k;

                reduced_idx := barycentric_indices_to_flat_index(reduced_degree, i, j, k);
                points_at_reduced_degree[reduced_idx] = blended;
            }
        }

        array_free(points_at_current_degree);
        points_at_current_degree = points_at_reduced_degree;
    }

    return points_at_current_degree[0];
}

/*

we sample the bezier triangle on a regular barycentric grid at the given resolution (number of subdivisions along each edge).
this is also known as tesselation (turning triangles into more triangles for more detail).

*/

create_bezier_triangle :: (bt: *Bezier_Triangle, resolution: int) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    n := resolution;
    inv_n := 1.0 / cast(float) n;

    /*
     evaluate surface positions at every barycentric grid point
     we must add them in the same order that control_point_flat_index
     expects: row 0 first (i = n), then row 1 (i = n-1), etc.
    
      row 0 (i=n):       one point    (n, 0, 0)
      row 1 (i=n-1):     two points   (n-1, 0, 1), (n-1, 1, 0)
      ...
      row r (i=n-r):     (r+1) points
      ...
      row n (i=0):       (n+1) points
    
     because we know that for barycentric indices (i, j, k) that i + j + k = n, then 
     we can simply turn them into coordiantes by doing:

         s = i/n, t = j/n, u = k/n

     so that s + t + u = 1.
    */

    for row: 0..n {
        i := n - row;
        for col: 0..row {
            j := col;
            k := row - col;

            s := cast(float) i * inv_n;
            t := cast(float) j * inv_n;
            u := cast(float) k * inv_n;

            pos := evaluate_bezier_triangle(bt, s, t, u);
            array_add(*result.positions, pos);
        }
    }

    // now the geometry is saved, we need to create indices about how to iterate over them to create triangles.
    // we iterate over barycentric indices (i, j, k) and for each point
    // emit an upward triangle and, where possible, a downward triangle.
    for i: 0..n - 1 { // this is from bottom to top of a triangle like this /\
        for j: 0..n - 1 - i {
            k := n - i - j;

            // upward triangle always occurs
            // Starting from (i, j, k), we reach the two neighbors by
            // adding 1 to j or i while subtracting 1 from k, keeping the sum = n.
            v0 := cast(u32) barycentric_indices_to_flat_index(n, i,     j,     k);
            v1 := cast(u32) barycentric_indices_to_flat_index(n, i,     j + 1, k - 1);
            v2 := cast(u32) barycentric_indices_to_flat_index(n, i + 1, j,     k - 1);

            array_add(*result.indices, v0);
            array_add(*result.indices, v1);
            array_add(*result.indices, v2);

            // downward triangle, if you think about k being straight up, then you can see that if you have a given 
            // barycentric index of (i, j, k), then if you go can go down 2, then you can form a up and then a down triangle
            // the down triangle has a line going staying on k - 1, and also has two lines going down to k - 2
            // note that this will account for all possible downward triangles. (it helps to draw this out)
            if k >= 2 {
                v3 := cast(u32) barycentric_indices_to_flat_index(n, i + 1, j,     k - 1);
                v4 := cast(u32) barycentric_indices_to_flat_index(n, i,     j + 1, k - 1);
                v5 := cast(u32) barycentric_indices_to_flat_index(n, i + 1, j + 1, k - 2);

                array_add(*result.indices, v3);
                array_add(*result.indices, v5);
                array_add(*result.indices, v4);
            }
        }
    }

    return result;
}
