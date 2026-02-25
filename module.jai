#import "Math";
#import "Basic";
#import "Random";

#import "tbx/turns";
#import "tbx/linear_algebra";
#import "tbx/math";


Indexed_Triangle_Positions :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
}
translate :: (itp: *Indexed_Triangle_Positions, offset: Vector3) {
    for *pos : itp.positions {
        pos.* += offset;
    }
}

translate_copy :: (itp: Indexed_Triangle_Positions, offset: Vector3) -> Indexed_Triangle_Positions {
    result : Indexed_Triangle_Positions;
    
    array_copy(*result.indices, itp.indices);
    array_copy(*result.positions, itp.positions);
    
    for *pos : result.positions {
        pos.* += offset;
    }
    
    return result;
}

Indexed_Triangle_Positions_Colors :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
    rgb_colors : [..] Vector3; 
}


/*

diffuse: to spread out in all directions

When light hits a surface, some of it bounces off directly (specular reflection)
and some penetrates the surface. The light that enters gets partially absorbed
and partially scattered back out in all directions — this is diffuse reflection.

The color we perceive is the light that was NOT absorbed during this process.
A white object absorbs very little; a red object absorbs most wavelengths
except red.

Most objects don't have a uniform diffuse value — it varies across the surface,
which is why we use diffuse maps (also called "base color" maps.

Note: call this albedo is wrong, learn about the definition of abledo, it is a value between 0-1 which is given to an entire object based on how it reflects light.

*/


Indexed_Mesh :: struct {
    name: string;
    indices             : [..] u32; 
    positions           : [..] Vector3; 
    normals             : [..] Vector3; 
    texture_coordinates : [..] Vector2; 
    rgb_colors          : [..] Vector3; 
    diffuse_map_path    : string; 
}

create_itpc_with_color :: (itp : Indexed_Triangle_Positions, color: Vector3) -> Indexed_Triangle_Positions_Colors {
    itpc : Indexed_Triangle_Positions_Colors;
    for itp.indices array_add(*itpc.indices, it);
    for itp.positions array_add(*itpc.positions, it);
    array_reserve(*itpc.rgb_colors, itpc.positions.count); 
    for 1..itpc.positions.count array_add(*itpc.rgb_colors, color);  
    return itpc;
}

random_get_vector_zero_to_one :: () -> Vector3 {
    return .{random_get_zero_to_one(), random_get_zero_to_one(), random_get_zero_to_one()};
}

create_itpc_with_color_variation :: (itp : Indexed_Triangle_Positions, color: Vector3 = NEGATIVE_ONE3, max_color_change :float = 0.2) -> Indexed_Triangle_Positions_Colors {

    itpc : Indexed_Triangle_Positions_Colors;
    for itp.indices array_add(*itpc.indices, it);
    for itp.positions array_add(*itpc.positions, it);
    array_reserve(*itpc.rgb_colors, itpc.positions.count); 

    base_color := color;
    if color == -ONE3 base_color = random_get_vector_zero_to_one();

    for 1..itpc.positions.count {

        // [-.5, .5] * 2 = [-1, 1] * scale = [-max_color_change, max_color_change];
        color_change := (random_get_vector_zero_to_one() - ONE3 * .5) * max_color_change;
        new_color := saturate(base_color + color_change);

        array_add(*itpc.rgb_colors, new_color);  
    }
    return itpc;
}


create_annulus :: (center: Vector3, normal: Vector3, inner_radius: float, outer_radius: float, num_segments: int) -> Indexed_Triangle_Positions {
    annulus : Indexed_Triangle_Positions;

    angle_step_turns :float = full_turn / num_segments;

    u, v: Vector3;
    u = arbitrary_unit_perpendicular(normal);
    v = normalized_cross(normal, u);

    for 0..num_segments-1 {
        next := (it + 1) % num_segments;

        t := it * angle_step_turns;

        dir := angle_to_unit_vector(t, u, v);

        inner_pos := center + dir * inner_radius;
        array_add(*annulus.positions, inner_pos);

        outer_pos := center + dir * outer_radius;
        array_add(*annulus.positions, outer_pos);

        // an annulus is just a quad strip that's connected end-to-end, so just make quads
        array_add(*annulus.indices, cast (u32)(2 * it));
        array_add(*annulus.indices, cast (u32)(2 * next));
        array_add(*annulus.indices, cast (u32)(2 * it + 1));

        array_add(*annulus.indices, cast (u32)(2 * next));
        array_add(*annulus.indices, cast (u32)(2 * next + 1));
        array_add(*annulus.indices, cast (u32)(2 * it + 1));
    }

    return annulus;
}

create_box :: (origin : Vector3 = .{0, 0, 0}, size_x : float = 1, size_y : float = 1, size_z : float = 1) -> Indexed_Triangle_Positions {
    box : Indexed_Triangle_Positions;

    half_x := size_x / 2;
    half_y := size_y / 2;
    half_z := size_z / 2;

    // 8 vertices
    //     4-------5
    //    /|      /|
    //   / |     / |
    //  7-------6  |
    //  |  0----|--1
    //  | /     | /
    //  |/      |/
    //  3-------2

    array_add(*box.positions, origin + Vector3.{-half_x, -half_y, -half_z}); // 0
    array_add(*box.positions, origin + Vector3.{ half_x, -half_y, -half_z}); // 1
    array_add(*box.positions, origin + Vector3.{ half_x, -half_y,  half_z}); // 2
    array_add(*box.positions, origin + Vector3.{-half_x, -half_y,  half_z}); // 3
    array_add(*box.positions, origin + Vector3.{-half_x,  half_y, -half_z}); // 4
    array_add(*box.positions, origin + Vector3.{ half_x,  half_y, -half_z}); // 5
    array_add(*box.positions, origin + Vector3.{ half_x,  half_y,  half_z}); // 6
    array_add(*box.positions, origin + Vector3.{-half_x,  half_y,  half_z}); // 7

    // 12 triangles (2 per face, 6 faces) - CCW winding, outward normals

    // bottom face (-y)
    array_add(*box.indices, 0); array_add(*box.indices, 3); array_add(*box.indices, 2);
    array_add(*box.indices, 0); array_add(*box.indices, 2); array_add(*box.indices, 1);

    // top face (+y)
    array_add(*box.indices, 4); array_add(*box.indices, 5); array_add(*box.indices, 6);
    array_add(*box.indices, 4); array_add(*box.indices, 6); array_add(*box.indices, 7);

    // front face (+z)
    array_add(*box.indices, 3); array_add(*box.indices, 7); array_add(*box.indices, 6);
    array_add(*box.indices, 3); array_add(*box.indices, 6); array_add(*box.indices, 2);

    // back face (-z)
    array_add(*box.indices, 1); array_add(*box.indices, 5); array_add(*box.indices, 4);
    array_add(*box.indices, 1); array_add(*box.indices, 4); array_add(*box.indices, 0);

    // left face (-x)
    array_add(*box.indices, 0); array_add(*box.indices, 4); array_add(*box.indices, 7);
    array_add(*box.indices, 0); array_add(*box.indices, 7); array_add(*box.indices, 3);

    // right face (+x)
    array_add(*box.indices, 1); array_add(*box.indices, 2); array_add(*box.indices, 6);
    array_add(*box.indices, 1); array_add(*box.indices, 6); array_add(*box.indices, 5);

    return box;
}

create_cylinder_between :: (p1: Vector3, p2: Vector3, segments: int, radius: float) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;
    
    axis := p2 - p1;
    up := unit_vector(axis);
    
    ortho, tangent := make_an_orthonormal_basis(up);
    
    angle_increment := cast(float)(2.0 * PI) / cast(float)segments;
    
    // top and bottom center vertices
    array_add(*result.positions, p2); // top center, index 0
    array_add(*result.positions, p1); // bottom center, index 1
    
    // vertices for top and bottom circles
    for i: 0..segments-1 {
        angle := cast(float)i * angle_increment;
        offset := radius * (cos(angle) * ortho + sin(angle) * tangent);
        
        array_add(*result.positions, p2 + offset); // top circle vertex
        array_add(*result.positions, p1 + offset); // bottom circle vertex
    }
    
    segments_2 := segments * 2;
    
    // indices for the top face
    for i: 0..segments-1 {
        array_add(*result.indices, 0);
        array_add(*result.indices, cast(u32)(2 + i * 2));
        array_add(*result.indices, cast(u32)(2 + ((i * 2 + 2) % segments_2)));
    }
    
    // indices for the bottom face
    for i: 0..segments-1 {
        array_add(*result.indices, 1);
        array_add(*result.indices, cast(u32)(3 + ((i * 2) % segments_2)));
        array_add(*result.indices, cast(u32)(3 + ((i * 2 + 2) % segments_2)));
    }
    
    // indices for the side faces
    for i: 0..segments-1 {
        top1    := cast(u32)(2 + (i * 2));
        bottom1 := cast(u32)(3 + (i * 2));
        top2    := cast(u32)(2 + ((i * 2 + 2) % segments_2));
        bottom2 := cast(u32)(3 + ((i * 2 + 2) % segments_2));
        
        array_add(*result.indices, top1);
        array_add(*result.indices, bottom1);
        array_add(*result.indices, top2);
        
        array_add(*result.indices, top2);
        array_add(*result.indices, bottom1);
        array_add(*result.indices, bottom2);
    }
    
    return result;
}

/*
 Create the points of an arrow pointing from one point to another

                                                g\
                                                | --\
                                                |    ---\
           |    a-------------------------------c        --\
 stem      |    |                    -------/   |           --\
 thickness |  start          -------/           |           -----end
           |    |    -------/                   |           --/
           |    b---/---------------------------d        --/
                                                |    ---/
                                                | --/
                                                f/

                                                ------tip len-------

 returns [a, b, c, d, e, f, g] from this we want to produce indices

 {a, b, c}, {c, b, d}, {e, f, g}
 {0, 1, 2}, {2, 1, 3}, {4, 5, 6}

 */
create_2d_arrow :: (start: Vector2, end: Vector2, stem_thickness: float, tip_fraction: float = 0.2) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    half_thickness := stem_thickness / 2.0;

    start_to_end := end - start;
    end_to_start := start - end;

    total_length := length(start_to_end);
    tip_length := tip_fraction * total_length;

    stem_length := max(0.0, total_length - tip_length);

    start_to_end_normalized := unit_vector(start_to_end);
    end_to_start_normalized := unit_vector(end_to_start);

    // note that these two are also normalized
    start_to_a_dir := Vector2.{-start_to_end_normalized.y, start_to_end_normalized.x};
    start_to_b_dir := Vector2.{start_to_end_normalized.y, -start_to_end_normalized.x};

    a := start + start_to_a_dir * half_thickness;
    b := start + start_to_b_dir * half_thickness;
    c := a + start_to_end_normalized * stem_length;
    d := b + start_to_end_normalized * stem_length;

    /*
     *
     *         (aln)--        |
     *               ----     |
     *                  ----  |
     *         (e2s)----------s-------------e
     *                  ----  |
     *               ----     |
     *         (bln)--        |
     *
     *         ------len 1-----
     */

    a_line := end_to_start_normalized + start_to_a_dir;
    b_line := end_to_start_normalized + start_to_b_dir;

    /*
     *       |o
     *       |    o    hyp = \sqrt{2} x
     *       x        o
     *       |            o
     *       |------x------- o
     */

    a_line = unit_vector(a_line) * sqrt(2.0) * tip_length;
    b_line = unit_vector(b_line) * sqrt(2.0) * tip_length;

    g := end + a_line;
    f := end + b_line;

    // positions
    array_add(*result.positions, Vector3.{a.x, a.y, 0.0});     // 0
    array_add(*result.positions, Vector3.{b.x, b.y, 0.0});     // 1
    array_add(*result.positions, Vector3.{c.x, c.y, 0.0});     // 2
    array_add(*result.positions, Vector3.{d.x, d.y, 0.0});     // 3
    array_add(*result.positions, Vector3.{end.x, end.y, 0.0}); // 4
    array_add(*result.positions, Vector3.{f.x, f.y, 0.0});     // 5
    array_add(*result.positions, Vector3.{g.x, g.y, 0.0});     // 6

    // indices: {a, b, c}, {c, b, d}, {e, f, g} => {0, 1, 2}, {2, 1, 3}, {4, 5, 6}
    for u32.[0, 1, 2, 2, 1, 3, 4, 5, 6] array_add(*result.indices, it);

    return result;
}
