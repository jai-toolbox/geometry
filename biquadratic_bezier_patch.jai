/*

A biquadratic Bezier patch is the tensor product of two quadratic Bezier curves.

Given a 3x3 grid of control points:

  p00  p01  p02
  p10  p11  p12
  p20  p21  p22

We evaluate it by first evaluating three quadratic Bezier curves along one axis (say columns),
giving us three intermediate points, and then evaluating a single quadratic Bezier curve along
the other axis through those three points.

A quadratic Bezier curve through three points a, b, c at parameter t is:

  Q(a, b, c, t) = (1-t)^2 * a + 2*(1-t)*t * b + t^2 * c

So the patch evaluation at (s, t) is:

  For each row r in {0, 1, 2}:
    temp[r] = Q(p[r][0], p[r][1], p[r][2], t)

  result = Q(temp[0], temp[1], temp[2], s)

Given a grid of size (width, height), there are (width - 1) / 2 *  (height - 1) / 2 patches.
Each patch starts at grid position (2n, 2m) and covers a 3 by  3 sub-grid. 

Adjacent patches share a column or row of three vertices along their common edge.

But that's not obvious yet, to see why we can make a few patches and see what they look like.

For example, a 5 by 3 grid has (5-1)/2 = 2 patches across and (3-1)/2 = 1 patch down:

    0   1   2   3   4
  0 *---*---*---*---*
    |   |   |   |   |
  1 *---P0--*---P1--*
    |   |   |   |   |
  2 *---*---*---*---*

  Patch 0 starts at (0, 0) and uses columns 0,1,2 and rows 0,1,2.
  Patch 1 starts at (0, 2) and uses columns 2,3,4 and rows 0,1,2.

  Column 2 (vertices (0,2), (1,2), (2,2)) is shared between both patches.

A 5 by 5 grid has 2 * 2 = 4 patches:

    0   1   2   3   4
  0 *---*---*---*---*
    |   |   |   |   |
  1 *---P0--*---P1--*
    |   |   |   |   |
  2 *---*---*---*---*
    |   |   |   |   |
  3 *---P2--*---P3--*
    |   |   |   |   |
  4 *---*---*---*---*


  Patch 0 starts at (0, 0) and uses columns 0,1,2 and rows 0,1,2.
  Patch 1 starts at (2, 0) and uses columns 2,3,4 and rows 0,1,2.
  Patch 2 starts at (0, 2) and uses columns 0,1,2 and rows 3,3,4.
  Patch 2 starts at (2, 0) and uses columns 0,1,2 and rows 3,3,4.

  Row 2 is shared vertically between the top and bottom patches.
  Column 2 is shared horizontally between the left and right patches.
  Vertex (2,2) is shared by all four patches.

  A 3 by 3 grid is the minimum case: just a single patch using all 9 vertices.

*/

Bezier_Patch_Grid :: struct (T: Type) {
    width:  int;
    height: int;
    control_points: [..] T;  // stored row-major: index = row * width + col
}

init_bezier_patch_grid :: (grid: *Bezier_Patch_Grid($T), width: int, height: int) {
    grid.width = width;
    grid.height = height;
    array_resize(*grid.control_points, width * height);
}

get_grid_control_point :: (grid: *Bezier_Patch_Grid($T), row: int, col: int) -> T {
    return grid.control_points[row * grid.width + col];
}

set_grid_control_point :: (grid: *Bezier_Patch_Grid($T), row: int, col: int, value: T) {
    grid.control_points[row * grid.width + col] = value;
}

// evaluates a single quadratic Bezier curve: Q(a, b, c, t) = (1-t)^2 * a + 2*(1-t)*t * b + t^2 * c
evaluate_quadratic_bezier :: (a: $T, b: T, c: T, t: float) -> T {
    one_minus_t := 1.0 - t;
    return one_minus_t * one_minus_t * a + 2.0 * one_minus_t * t * b + t * t * c;
}

/*
evaluates a single biquadratic Bezier patch given its 3x3 control points at parameters (s, t)
where s runs along rows and t runs along columns, both in [0, 1]
patch_controls is a 3x3 array stored row-major (9 elements)
*/
evaluate_biquadratic_patch :: (patch_controls: [] $T, s: float, t: float) -> T {

    // evaluate along columns for each row
    temp0 := evaluate_quadratic_bezier(patch_controls[0], patch_controls[1], patch_controls[2], t);
    temp1 := evaluate_quadratic_bezier(patch_controls[3], patch_controls[4], patch_controls[5], t);
    temp2 := evaluate_quadratic_bezier(patch_controls[6], patch_controls[7], patch_controls[8], t);

    // evaluate along rows
    return evaluate_quadratic_bezier(temp0, temp1, temp2, s);
}

/*

Tessellate a single biquadratic Bezier patch into a triangle mesh.

resolution is the number of subdivisions along each edge of the patch, so we get
(resolution + 1) x (resolution + 1) vertices and resolution x resolution x 2 triangles (2 tris per quad.)

patch_controls: the 3x3 control points in row-major order.

*/

Tessellated_Patch_Result :: struct (T: Type) {
    values:  [..] T;
    indices: [..] u32;
}

tessellate_biquadratic_patch :: (patch_controls: [] $T, resolution: int) -> Tessellated_Patch_Result(T) {
    result: Tessellated_Patch_Result(T);

    n := resolution;
    inv_n := 1.0 / cast(float) n;
    verts_per_side := n + 1;

    // evaluate surface values on a regular grid
    for row: 0..n {
        s := cast(float) row * inv_n;
        for col: 0..n {
            t := cast(float) col * inv_n;
            val := evaluate_biquadratic_patch(patch_controls, s, t);
            array_add(*result.values, val);
        }
    }

    // emit two triangles per quad cell
    for row: 0..n - 1 {
        for col: 0..n - 1 {
            // vertices of the quad
            top_left     := cast(u32) (row       * verts_per_side + col);
            top_right    := cast(u32) (row       * verts_per_side + col + 1);
            bottom_left  := cast(u32) ((row + 1) * verts_per_side + col);
            bottom_right := cast(u32) ((row + 1) * verts_per_side + col + 1);

            // triangle 1 (upper-left)
            array_add(*result.indices, top_left);
            array_add(*result.indices, bottom_left);
            array_add(*result.indices, top_right);

            // triangle 2 (lower-right)
            array_add(*result.indices, top_right);
            array_add(*result.indices, bottom_left);
            array_add(*result.indices, bottom_right);
        }
    }

    return result;
}

/*

Tessellate the entire grid of biquadratic Bezier patches into a single triangle mesh.

The grid contains (width - 1) / 2 by (height - 1) / 2 patches, each starting at
grid position (2n, 2m) and covering a 3x3 sub-grid of control vertices.

resolution is the number of subdivisions per patch edge.

To avoid duplicate vertices along shared edges between adjacent patches, we allocate
a global vertex grid and fill it in patch by patch, only writing a vertex if it hasn't
been written yet.

*/

create_bezier_patch_grid :: (grid: *Bezier_Patch_Grid($T), resolution: int) -> Tessellated_Patch_Result(T) {
    result: Tessellated_Patch_Result(T);

    num_patches_x := (grid.width  - 1) / 2;
    num_patches_y := (grid.height - 1) / 2;

    n := resolution;
    inv_n := 1.0 / cast(float) n;

    // total number of unique vertices across all patches when shared edges are merged:
    // each patch contributes (n+1) vertices along each axis, but adjacent patches
    // share one column/row of vertices.
    total_verts_x := num_patches_x * n + 1;
    total_verts_y := num_patches_y * n + 1;
    total_verts   := total_verts_x * total_verts_y;

    array_resize(*result.values, total_verts);

    // we use a flag array to avoid writing the same vertex twice (shared edges)
    written: [..] bool;
    array_resize(*written, total_verts);
    for 0..total_verts - 1  written[it] = false;
    defer array_free(written);

    // iterate over each patch
    for py: 0..num_patches_y - 1 {
        for px: 0..num_patches_x - 1 {
            // starting control point indices in the grid
            gi := 2 * py;  // row in control point grid
            gj := 2 * px;  // col in control point grid

            // extract the 3x3 control points for this patch (row-major)
            patch_controls: [9] T;
            for r: 0..2 {
                for c: 0..2 {
                    patch_controls[r * 3 + c] = get_grid_control_point(grid, gi + r, gj + c);
                }
            }

            // starting vertex indices in the global tessellation grid
            base_vx := px * n;
            base_vy := py * n;

            // evaluate and store vertices
            for row: 0..n {
                s := cast(float) row * inv_n;
                for col: 0..n {
                    t := cast(float) col * inv_n;

                    vx := base_vx + col;
                    vy := base_vy + row;
                    global_idx := vy * total_verts_x + vx;

                    if !written[global_idx] {
                        val := evaluate_biquadratic_patch(patch_controls, s, t);
                        result.values[global_idx] = val;
                        written[global_idx] = true;
                    }
                }
            }

            // emit triangle indices
            for row: 0..n - 1 {
                for col: 0..n - 1 {
                    vx := base_vx + col;
                    vy := base_vy + row;

                    top_left     := cast(u32) (vy       * total_verts_x + vx);
                    top_right    := cast(u32) (vy       * total_verts_x + vx + 1);
                    bottom_left  := cast(u32) ((vy + 1) * total_verts_x + vx);
                    bottom_right := cast(u32) ((vy + 1) * total_verts_x + vx + 1);

                    // triangle 1
                    array_add(*result.indices, top_left);
                    array_add(*result.indices, bottom_left);
                    array_add(*result.indices, top_right);

                    // triangle 2
                    array_add(*result.indices, top_right);
                    array_add(*result.indices, bottom_left);
                    array_add(*result.indices, bottom_right);
                }
            }
        }
    }

    return result;
}
