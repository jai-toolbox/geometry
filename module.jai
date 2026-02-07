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

Indexed_Triangle_Positions_Colors :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
    rgb_colors : [..] Vector3; 
}

Indexed_Mesh :: struct {
    indices             : [..] u32; 
    positions           : [..] Vector3; 
    normals             : [..] Vector3; 
    texture_coordinates : [..] Vector2; 
    rgb_colors          : [..] Vector3; 
    name: string;
}

create_itpc_with_color :: (itp : Indexed_Triangle_Positions, color: Vector3) -> Indexed_Triangle_Positions_Colors {
    itpc : Indexed_Triangle_Positions_Colors;
    for itp.indices array_add(*itpc.indices, it);
    for itp.positions array_add(*itpc.positions, it);
    array_reserve(*itpc.rgb_colors, itpc.positions.count); 
    for 1..itpc.positions.count array_add(*itpc.rgb_colors, color);  
    return itpc;
}

create_itpc_with_color_variation :: (itp : Indexed_Triangle_Positions, color: Vector3) -> Indexed_Triangle_Positions_Colors {
    itpc : Indexed_Triangle_Positions_Colors;
    for itp.indices array_add(*itpc.indices, it);
    for itp.positions array_add(*itpc.positions, it);
    array_reserve(*itpc.rgb_colors, itpc.positions.count); 
    for 1..itpc.positions.count {
        scale := 1.0;
        r1 := random_get_zero_to_one();
        r2 := random_get_zero_to_one();
        r3 := random_get_zero_to_one();
        random_color_offset := Vector3.{r1, r2, r3} * scale ;
        array_add(*itpc.rgb_colors, color + random_color_offset);  
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

generate_box :: (origin : Vector3, size_x : float, size_y : float, size_z : float) -> Indexed_Triangle_Positions {
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
