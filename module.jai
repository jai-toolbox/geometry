#import "Math";
#import "Basic";
#import "Random";

#import "tbx/turns";
#import "tbx/linear_algebra";
#import "tbx/math";
#import "tbx/rectangle";

Indexed_Triangle_Positions :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
}

deinit :: (using itp: *Indexed_Triangle_Positions) {
    array_free(indices);
    array_free(positions);
}

merge :: (dest: *Indexed_Triangle_Positions, source: Indexed_Triangle_Positions) {
    base_idx := cast(u32) dest.positions.count;
    
    for source.positions  array_add(*dest.positions, it);
    for source.indices    array_add(*dest.indices, it + base_idx);
}

Indexed_Triangle_Position_Normals :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
    normals : [..] Vector3; 
}

deinit :: (using itpn: *Indexed_Triangle_Position_Normals) {
    array_free(indices);
    array_free(positions);
    array_free(normals);
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

polygon_normal :: (positions : [] Vector3) -> Vector3 {
    normal := ZERO3;

    for i : 0..positions.count-1 {
        current := positions[i];
        next := positions[(i + 1) % positions.count];

        normal.x += (current.y - next.y) * (current.z + next.z);
        normal.y += (current.z - next.z) * (current.x + next.x);
        normal.z += (current.x - next.x) * (current.y + next.y);
    }

    return normal;
}

dominant_axis_to_drop :: (normal : Vector3) -> int {
    abs_x := abs(normal.x);
    abs_y := abs(normal.y);
    abs_z := abs(normal.z);

    if abs_x >= abs_y && abs_x >= abs_z return 0;
    if abs_y >= abs_z return 1;
    return 2;
}

project_for_triangulation :: (position : Vector3, axis_to_drop : int) -> Vector2 {
    if axis_to_drop == 0 return .{position.y, position.z};
    if axis_to_drop == 1 return .{position.x, position.z};
    return .{position.x, position.y};
}

cross_2d :: (a : Vector2, b : Vector2, c : Vector2) -> float {
    ab := b - a;
    ac := c - a;
    return ab.x * ac.y - ab.y * ac.x;
}

signed_polygon_area_2d :: (positions : [] Vector2) -> float {
    area := 0.0;
    for i : 0..positions.count-1 {
        current := positions[i];
        next := positions[(i + 1) % positions.count];
        area += current.x * next.y - next.x * current.y;
    }
    return area * 0.5;
}

point_in_triangle_2d :: (point : Vector2, a : Vector2, b : Vector2, c : Vector2, ccw : bool) -> bool {
    epsilon :: 0.000001;

    ab := cross_2d(a, b, point);
    bc := cross_2d(b, c, point);
    ca := cross_2d(c, a, point);

    if ccw {
        return ab >= -epsilon && bc >= -epsilon && ca >= -epsilon;
    } else {
        return ab <= epsilon && bc <= epsilon && ca <= epsilon;
    }
}

triangulate_polygon_fan :: (positions : [] Vector3) -> [..] u32 {
    indices : [..] u32;
    if positions.count < 3 return indices;

    array_reserve(*indices, (positions.count - 2) * 3);

    for i : 1..positions.count-2 {
        array_add(*indices, 0);
        array_add(*indices, cast(u32)i);
        array_add(*indices, cast(u32)(i + 1));
    }

    return indices;
}

triangulate_polygon :: (positions : [] Vector3) -> [..] u32 {
    if positions.count < 3 {
        indices : [..] u32;
        return indices;
    }

    if positions.count == 3 return triangulate_polygon_fan(positions);

    projected : [..] Vector2;
    projected.allocator = temp;
    array_reserve(*projected, positions.count);

    normal := polygon_normal(positions);
    axis_to_drop := dominant_axis_to_drop(normal);

    for position : positions {
        array_add(*projected, project_for_triangulation(position, axis_to_drop));
    }

    ccw := signed_polygon_area_2d(projected) > 0;

    remaining : [..] int;
    remaining.allocator = temp;
    array_reserve(*remaining, positions.count);
    for i : 0..positions.count-1 array_add(*remaining, i);

    triangulated_indices : [..] u32;
    array_reserve(*triangulated_indices, (positions.count - 2) * 3);

    guard := 0;
    while remaining.count > 3 {
        ear_found := false;

        for remaining_index : 0..remaining.count-1 {
            prev_index := remaining[(remaining_index + remaining.count - 1) % remaining.count];
            current_index := remaining[remaining_index];
            next_index := remaining[(remaining_index + 1) % remaining.count];

            a := projected[prev_index];
            b := projected[current_index];
            c := projected[next_index];

            corner_cross := cross_2d(a, b, c);
            if ccw {
                if corner_cross <= 0.000001 continue;
            } else {
                if corner_cross >= -0.000001 continue;
            }

            contains_other_vertex := false;
            for test_index : remaining {
                if test_index == prev_index || test_index == current_index || test_index == next_index continue;
                if point_in_triangle_2d(projected[test_index], a, b, c, ccw) {
                    contains_other_vertex = true;
                    break;
                }
            }

            if contains_other_vertex continue;

            array_add(*triangulated_indices, cast(u32)prev_index);
            array_add(*triangulated_indices, cast(u32)current_index);
            array_add(*triangulated_indices, cast(u32)next_index);
            array_ordered_remove_by_index(*remaining, remaining_index);
            ear_found = true;
            break;
        }

        if !ear_found {
            array_free(triangulated_indices);
            return triangulate_polygon_fan(positions);
        }

        guard += 1;
        if guard > positions.count * positions.count {
            array_free(triangulated_indices);
            return triangulate_polygon_fan(positions);
        }
    }

    if remaining.count == 3 {
        array_add(*triangulated_indices, cast(u32)remaining[0]);
        array_add(*triangulated_indices, cast(u32)remaining[1]);
        array_add(*triangulated_indices, cast(u32)remaining[2]);
    }

    return triangulated_indices;
}

append_triangulated_polygon_indices :: (indices : *[..] u32, positions : [] Vector3, first_index : u32, count : int) {
    if count < 3 return;

    polygon_positions : [..] Vector3;
    polygon_positions.allocator = temp;
    array_reserve(*polygon_positions, count);

    for i : 0..count-1 {
        array_add(*polygon_positions, positions[first_index + cast(u32)i]);
    }

    local_indices := triangulate_polygon(polygon_positions);
    defer array_free(local_indices);

    for local_indices {
        array_add(indices, first_index + it);
    }
}



Indexed_Triangle_Positions_Colors :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
    rgb_colors : [..] Vector3; 
}

deinit :: (using itpc: *Indexed_Triangle_Positions_Colors) {
    array_free(indices);
    array_free(positions);
    array_free(rgb_colors);
}

Indexed_Triangle_Positions_Color_Alphas :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
    rgba_colors : [..] Vector4; 
}

merge :: (dest: *Indexed_Triangle_Positions_Color_Alphas, source: Indexed_Triangle_Positions_Color_Alphas) {
    base_idx := cast(u32) dest.positions.count;
    
    for source.positions    array_add(*dest.positions, it);
    for source.rgba_colors  array_add(*dest.rgba_colors, it);
    for source.indices      array_add(*dest.indices, it + base_idx);
}

merge :: (dest: *Indexed_Triangle_Positions_Colors, source: Indexed_Triangle_Positions_Colors) {
    base_idx := cast(u32) dest.positions.count;
    
    for source.positions  array_add(*dest.positions, it);
    for source.rgb_colors array_add(*dest.rgb_colors, it);
    for source.indices    array_add(*dest.indices, it + base_idx);
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


create_itpc_with_color :: (itp: Indexed_Triangle_Positions, color: Vector3) -> Indexed_Triangle_Positions_Colors {
    itpc: Indexed_Triangle_Positions_Colors;
    itpc.indices = itp.indices;
    itpc.positions = itp.positions;
    array_reserve(*itpc.rgb_colors, itpc.positions.count);
    for 1..itpc.positions.count  array_add(*itpc.rgb_colors, color);
    return itpc;
}

// do we ever even need the copy functions here?, maybe we can just remove them
create_itpc_with_color_copy :: (itp: Indexed_Triangle_Positions, color: Vector3) -> Indexed_Triangle_Positions_Colors {
    itpc: Indexed_Triangle_Positions_Colors;
    array_copy(*itpc.indices, itp.indices);
    array_copy(*itpc.positions, itp.positions);
    array_reserve(*itpc.rgb_colors, itpc.positions.count);
    for 1..itpc.positions.count array_add(*itpc.rgb_colors, color);
    return itpc;
}

create_itpca_with_color_alpha :: (itp : Indexed_Triangle_Positions, color_alpha: Vector4) -> Indexed_Triangle_Positions_Color_Alphas {
    itpc : Indexed_Triangle_Positions_Color_Alphas;
    itpc.indices = itp.indices;
    itpc.positions = itp.positions;
    array_reserve(*itpc.rgba_colors, itpc.positions.count);
    for 1..itpc.positions.count array_add(*itpc.rgba_colors, color_alpha);
    return itpc;
}

create_itpca_with_color_alpha_copy :: (itp : Indexed_Triangle_Positions, color_alpha: Vector4) -> Indexed_Triangle_Positions_Color_Alphas {
    itpc : Indexed_Triangle_Positions_Color_Alphas;
    array_copy(itpc.indices, itp.indices);
    array_copy(itpc.positions, itp.positions);
    array_reserve(*itpc.rgba_colors, itpc.positions.count);
    for 1..itpc.positions.count array_add(*itpc.rgba_colors, color_alpha);
    return itpc;
}

random_get_vector_zero_to_one :: () -> Vector3 {
    return .{random_get_zero_to_one(), random_get_zero_to_one(), random_get_zero_to_one()};
}

fill_color_variation :: (itpc: *Indexed_Triangle_Positions_Colors, color: Vector3, max_color_change: float) {
    base_color := color;
    if color == -ONE3  base_color = random_get_vector_zero_to_one();

    array_reserve(*itpc.rgb_colors, itpc.positions.count);
    for 1..itpc.positions.count {
        color_change := (random_get_vector_zero_to_one() - ONE3 * 0.5) * max_color_change;
        array_add(*itpc.rgb_colors, saturate(base_color + color_change));
    }
}

create_itpc_with_color_variation :: (itp: Indexed_Triangle_Positions, color: Vector3 = NEGATIVE_ONE3, max_color_change: float = 0.2) -> Indexed_Triangle_Positions_Colors {
    itpc: Indexed_Triangle_Positions_Colors;
    itpc.indices   = itp.indices;
    itpc.positions = itp.positions;
    fill_color_variation(*itpc, color, max_color_change);
    return itpc;
}

create_itpc_with_color_variation_copy :: (itp: Indexed_Triangle_Positions, color: Vector3 = NEGATIVE_ONE3, max_color_change: float = 0.2) -> Indexed_Triangle_Positions_Colors {
    itpc: Indexed_Triangle_Positions_Colors;
    array_copy(*itpc.indices,   itp.indices);
    array_copy(*itpc.positions, itp.positions);
    fill_color_variation(*itpc, color, max_color_change);
    return itpc;
}

create_rectangle :: (center: Vector3, center_to_edge_a: Vector3, center_to_edge_b: Vector3) -> Indexed_Triangle_Positions {
    rect : Indexed_Triangle_Positions;

    assert(abs(cos_of_angle_between(center_to_edge_a, center_to_edge_b)) < 0.0001, "center_to_edge_a and center_to_edge_b must be perpendicular");

    array_add(*rect.positions, center - center_to_edge_a - center_to_edge_b);
    array_add(*rect.positions, center + center_to_edge_a - center_to_edge_b);
    array_add(*rect.positions, center + center_to_edge_a + center_to_edge_b);
    array_add(*rect.positions, center - center_to_edge_a + center_to_edge_b);

    // triangle 1
    array_add(*rect.indices, cast(u32) 0);
    array_add(*rect.indices, cast(u32) 1);
    array_add(*rect.indices, cast(u32) 2);

    // triangle 2
    array_add(*rect.indices, cast(u32) 0);
    array_add(*rect.indices, cast(u32) 2);
    array_add(*rect.indices, cast(u32) 3);

    return rect;
}

create_circle :: (center: Vector3, normal: Vector3, radius: float, num_segments: int) -> Indexed_Triangle_Positions {
    circle : Indexed_Triangle_Positions;

    angle_step_turns : float = FULL_TURN / num_segments;

    u, v: Vector3;
    u = arbitrary_unit_perpendicular(normal);
    v = normalized_cross(normal, u);

    // center vertex at index 0
    array_add(*circle.positions, center);

    for 0..num_segments-1 {
        t := it * angle_step_turns;
        dir := angle_to_unit_vector(t, u, v);
        array_add(*circle.positions, center + dir * radius);
    }

    // triangle fan from center
    for 0..num_segments-1 {
        next := (it + 1) % num_segments;

        array_add(*circle.indices, 0);                      // center
        array_add(*circle.indices, cast(u32)(1 + it));      // current
        array_add(*circle.indices, cast(u32)(1 + next));    // next
    }

    return circle;
}


create_annulus :: (center: Vector3, normal: Vector3, inner_radius: float, outer_radius: float, num_segments: int) -> Indexed_Triangle_Positions {
    annulus : Indexed_Triangle_Positions;

    angle_step_turns :float = FULL_TURN / num_segments;

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

create_cube :: (origin: Vector3 = .{0, 0, 0}, side_length: float) -> Indexed_Triangle_Positions {
    return create_box(origin, side_length, side_length, side_length);
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

create_sphere :: (
    center: Vector3 = .{0, 0, 0},
    radius: float = 1.0,
    latitude_segments: int = 16,
    longitude_segments: int = 32
) -> Indexed_Triangle_Positions {
    return create_ellipsoid(center, Vector3.{radius, radius, radius}, latitude_segments, longitude_segments);
}

create_ellipsoid :: (
    center: Vector3 = .{0, 0, 0},
    radii: Vector3 = .{1, 1, 1},
    latitude_segments: int = 16,
    longitude_segments: int = 32
) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    assert(radii.x >= 0.0 && radii.y >= 0.0 && radii.z >= 0.0, "radii must be non-negative");
    assert(latitude_segments >= 2, "latitude_segments must be at least 2");
    assert(longitude_segments >= 3, "longitude_segments must be at least 3");

    top_index: u32 = 0;
    array_add(*result.positions, center + Vector3.{0, radii.y, 0});

    latitude_step := (FULL_TURN / 2.0) / cast(float) latitude_segments;
    longitude_step := FULL_TURN / cast(float) longitude_segments;

    for lat: 1..latitude_segments-1 {
        theta := cast(float) lat * latitude_step;
        ring_radius := sin_turns(theta);
        y := cos_turns(theta) * radii.y;

        for lon: 0..longitude_segments-1 {
            phi := cast(float) lon * longitude_step;

            array_add(*result.positions, center + Vector3.{
                cos_turns(phi) * ring_radius * radii.x,
                y,
                sin_turns(phi) * ring_radius * radii.z
            });
        }
    }

    bottom_index := cast(u32) result.positions.count;
    array_add(*result.positions, center + Vector3.{0, -radii.y, 0});

    first_ring_index: u32 = 1;
    last_ring_index := cast(u32)(1 + (latitude_segments - 2) * longitude_segments);

    for lon: 0..longitude_segments-1 {
        next := (lon + 1) % longitude_segments;

        array_add(*result.indices, top_index);
        array_add(*result.indices, first_ring_index + cast(u32) next);
        array_add(*result.indices, first_ring_index + cast(u32) lon);
    }

    for lat: 0..latitude_segments-3 {
        current_ring := cast(u32)(1 + lat * longitude_segments);
        next_ring := cast(u32)(1 + (lat + 1) * longitude_segments);

        for lon: 0..longitude_segments-1 {
            next_lon := (lon + 1) % longitude_segments;

            v00 := current_ring + cast(u32) lon;
            v01 := current_ring + cast(u32) next_lon;
            v10 := next_ring + cast(u32) lon;
            v11 := next_ring + cast(u32) next_lon;

            array_add(*result.indices, v00);
            array_add(*result.indices, v01);
            array_add(*result.indices, v11);

            array_add(*result.indices, v00);
            array_add(*result.indices, v11);
            array_add(*result.indices, v10);
        }
    }

    for lon: 0..longitude_segments-1 {
        next := (lon + 1) % longitude_segments;

        array_add(*result.indices, last_ring_index + cast(u32) lon);
        array_add(*result.indices, last_ring_index + cast(u32) next);
        array_add(*result.indices, bottom_index);
    }

    return result;
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

create_torus :: (
    center: Vector3 = .{0, 0, 0},
    normal: Vector3 = .{0, 0, 1},       // axis the torus is oriented around
    major_radius: float,                 // distance from center to tube center
    minor_radius: float,                 // radius of the tube itself
    major_segments: int,                 // segments around the main ring
    minor_segments: int                  // segments around the tube
) -> Indexed_Triangle_Positions {

    result: Indexed_Triangle_Positions;

    // build an orthonormal basis from the normal
    u := arbitrary_unit_perpendicular(normal);
    v := normalized_cross(normal, u);

    major_step := FULL_TURN / cast(float) major_segments;
    minor_step := FULL_TURN / cast(float) minor_segments;

    // generate vertices
    // for each point on the major ring, generate a ring of minor_segments vertices around the tube
    for maj: 0..major_segments-1 {
        major_angle := cast(float) maj * major_step;

        // point on the major ring (center of this tube cross-section)
        ring_center := center
            + major_radius * (cos_turns(major_angle) * u + sin_turns(major_angle) * v);

        // outward direction from the torus axis to this ring center
        radial_dir := unit_vector(ring_center - center);

        for min: 0..minor_segments-1 {
            minor_angle := cast(float) min * minor_step;

            // position on the tube cross-section
            pos := ring_center
                + minor_radius * (cos_turns(minor_angle) * radial_dir
                                + sin_turns(minor_angle) * normal);

            array_add(*result.positions, pos);
        }
    }

    // generate indices
    // vertex index: maj * minor_segments + min
    for maj: 0..major_segments-1 {
        next_maj := (maj + 1) % major_segments;

        for min: 0..minor_segments-1 {
            next_min := (min + 1) % minor_segments;

            v00 := cast(u32)(maj      * minor_segments + min);
            v10 := cast(u32)(next_maj * minor_segments + min);
            v01 := cast(u32)(maj      * minor_segments + next_min);
            v11 := cast(u32)(next_maj * minor_segments + next_min);

            // quad as two CCW triangles
            array_add(*result.indices, v00);
            array_add(*result.indices, v10);
            array_add(*result.indices, v11);

            array_add(*result.indices, v00);
            array_add(*result.indices, v11);
            array_add(*result.indices, v01);
        }
    }

    return result;
}

create_cone :: (
    tip: Vector3,
    base_center: Vector3,
    radius: float,
    segments: int
) -> Indexed_Triangle_Positions {

    result: Indexed_Triangle_Positions;

    axis := base_center - tip;
    up   := unit_vector(axis);

    u := arbitrary_unit_perpendicular(up);
    v  := normalized_cross(up, u);

    angle_step := FULL_TURN / cast(float) segments;

    // tip vertex at index 0
    array_add(*result.positions, tip);

    // base center vertex at index 1
    array_add(*result.positions, base_center);

    // base ring vertices starting at index 2
    for i: 0..segments-1 {
        angle := cast(float) i * angle_step;
        offset := radius * (cos_turns(angle) * u + sin_turns(angle) * v);
        array_add(*result.positions, base_center + offset);
    }

    for i: 0..segments-1 {
        curr := cast(u32)(2 + i);
        next := cast(u32)(2 + (i + 1) % segments);

        // side face: tip -> next -> curr (CCW from outside)
        array_add(*result.indices, 0);
        array_add(*result.indices, next);
        array_add(*result.indices, curr);

        // base face: base_center -> curr -> next (CCW looking from below)
        array_add(*result.indices, 1);
        array_add(*result.indices, curr);
        array_add(*result.indices, next);
    }

    return result;
}

create_3d_arrow :: (start: Vector3, end: Vector3, num_segments: int, stem_thickness: float) -> Indexed_Triangle_Positions {
    dir := end - start;
    len := length(dir);

    tip_length := 0.3 * len;
    tip_base   := end - unit_vector(dir) * tip_length;

    stem := create_cylinder_between(start, tip_base, num_segments, stem_thickness);
    tip  := create_cone(end, tip_base, stem_thickness * 2.0, num_segments);

    merge(*stem, tip);
    return stem;
}

create_3d_arrow_with_ratio :: (start: Vector3, end: Vector3, num_segments: int = 8, length_thickness_ratio: float = 0.1) -> Indexed_Triangle_Positions {
    len             := length(end - start);
    stem_thickness  := len * length_thickness_ratio;
    return create_3d_arrow(start, end, num_segments, stem_thickness);
}

connect_points_by_arrows :: (points: [] Vector3, num_segments: int = 8, length_thickness_ratio: float = 0.1) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    if points.count < 2  return result;

    num_consecutive_identical := 0;
    last_non_zero_dir : Vector3;
    has_last_dir      := false;

    for i: 0..points.count-2 {
        start := points[i];
        end   := points[i + 1];

        if start == end {
            num_consecutive_identical += 1;
            if has_last_dir {
                radius := cast(float) num_consecutive_identical / 100.0;
                marker := create_circle(start, unit_vector(last_non_zero_dir), radius, 5);
                merge(*result, marker);
            }
        } else {
            num_consecutive_identical = 0;
            last_non_zero_dir = end - start;
            has_last_dir      = true;

            seg := create_3d_arrow_with_ratio(start, end, num_segments, length_thickness_ratio);
            merge(*result, seg);
        }
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

/*

This function creates a graph, it takes a bunch of 2d points like this:

|                    .
|           .         
|                     
|     .        .      
|  .                  
|        .        .   
|                     
|.............................

this function takes the aobve graph and "supports it from below" with rectangles:

|                    .
|           .        |
|           |        |
|     .     |...     |
|  ...|     |  |     |
|  |  |.....|  |.....|
|  |  |  |  |  |  |  |
|.............................

note: the rectangle height is always the min height of the two sequential points.

The function then adds triangles on top of each rectangle to connect the points linearly. "thickening it up".

*/
create_graph :: (points: [] Vector2, origin: Vector2 = .{0, 0}, scale: Vector2 = .{1, 1}) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;
    
    if points.count < 2 return result;
    
    for i: 0..points.count-2 {
        p1 := points[i];
        p2 := points[i + 1];
        
        x1 := origin.x + p1.x * scale.x;
        y1 := origin.y + p1.y * scale.y;
        x2 := origin.x + p2.x * scale.x;
        y2 := origin.y + p2.y * scale.y;
        y0 := origin.y;
        
        y_min := min(y1, y2);
        
        base_idx := cast(u32) result.positions.count;
        
        v_tl := Vector3.{x1, y_min, 0};  // base_idx + 0
        v_tr := Vector3.{x2, y_min, 0};  // base_idx + 1
        v_bl := Vector3.{x1, y0,    0};  // base_idx + 2
        v_br := Vector3.{x2, y0,    0};  // base_idx + 3
        v_p1 := Vector3.{x1, y1,    0};  // base_idx + 4
        v_p2 := Vector3.{x2, y2,    0};  // base_idx + 5
        
        array_add(*result.positions, v_tl);
        array_add(*result.positions, v_tr);
        array_add(*result.positions, v_bl);
        array_add(*result.positions, v_br);
        array_add(*result.positions, v_p1);
        array_add(*result.positions, v_p2);
        
        // Rectangle: tl, bl, br, tr
        array_add(*result.indices, base_idx + 0);
        array_add(*result.indices, base_idx + 2);
        array_add(*result.indices, base_idx + 3);
        
        array_add(*result.indices, base_idx + 0);
        array_add(*result.indices, base_idx + 3);
        array_add(*result.indices, base_idx + 1);
        
        /*
         right-angle triangle on top of the rectangle

         when y1 > y2 (decreasing): triangle is p1(x1,y1), tl(x1,y_min), tr(x2,y_min)
           right angle at tl = (x1, y_min) = (x1, y2)

         when y2 > y1 (increasing): triangle is tl(x1,y_min), tr(x2,y_min), p2(x2,y2)
           right angle at tr = (x2, y_min) = (x2, y1)

         when y1 == y2: triangle is degenerate (zero area), harmless
        */
        
        array_add(*result.indices, base_idx + 4); // p1
        array_add(*result.indices, base_idx + 0); // tl
        array_add(*result.indices, base_idx + 1); // tr
        
        array_add(*result.indices, base_idx + 1); // tr
        array_add(*result.indices, base_idx + 5); // p2
        array_add(*result.indices, base_idx + 4); // p1
    }
    
    return result;
}

rectangle_to_itp :: (rectangle: Rectangle, z: float = 0.0) -> Indexed_Triangle_Positions {
    half_w := rectangle.w / 2.0;
    half_h := rectangle.h / 2.0;

    x0 := rectangle.x - half_w;
    y0 := rectangle.y - half_h;
    x1 := rectangle.x + half_w;
    y1 := rectangle.y + half_h;

    return create_quad(x0, y0, x1, y1, z);
}

create_quad :: (x0: float = -1.0, y0: float = -1.0, x1: float = 1.0, y1: float = 1.0, z: float = 0.0) -> Indexed_Triangle_Positions {
    itp: Indexed_Triangle_Positions;
    
    array_add(*itp.positions, Vector3.{x0, y0, z});
    array_add(*itp.positions, Vector3.{x1, y0, z});
    array_add(*itp.positions, Vector3.{x1, y1, z});
    array_add(*itp.positions, Vector3.{x0, y1, z});
    
    array_add(*itp.indices, 0);
    array_add(*itp.indices, 1);
    array_add(*itp.indices, 2);
    
    array_add(*itp.indices, 0);
    array_add(*itp.indices, 2);
    array_add(*itp.indices, 3);
    
    return itp;
}


create_star :: (center_x: float, center_y: float, outer_radius: float, inner_radius: float, num_star_tips: int, blunt_tips: bool = false) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    star_multiplier := ifx blunt_tips then 3 else 2;
    num_vertices := star_multiplier * num_star_tips;

    inner_radius_offset := 1;

    full_rotation := cast(float)(2.0 * PI);
    angle_step := full_rotation / cast(float) num_vertices;

    // make sure that a tip always faces directly up
    initial_angle := full_rotation / 4.0;

    // align the flat part of the blunt tip so that it points vertically
    if blunt_tips {
        initial_angle -= angle_step / 2.0;
    }

    // generate vertices
    for i: 0..num_vertices - 1 {
        angle := initial_angle + cast(float) i * angle_step;

        radius: float;
        if (i + inner_radius_offset) % star_multiplier == 0 {
            radius = inner_radius;
        } else {
            radius = outer_radius;
        }

        array_add(*result.positions, Vector3.{
            center_x + radius * cos_turns(angle),
            center_y + radius * sin_turns(angle),
            0.0,
        });
    }

    // the center vertex is required to form triangles and is stored at the very end
    array_add(*result.positions, Vector3.{center_x, center_y, 0.0});

    // generate indices — triangle fan from center
    for i: 0..num_vertices - 1 {
        next := (i + 1) % num_vertices;
        array_add(*result.indices, cast(u32) i);
        array_add(*result.indices, cast(u32) next);
        array_add(*result.indices, cast(u32) num_vertices); // center index
    }

    return result;
}


#load "bezier_triangle.jai";
#load "biquadratic_bezier_patch.jai";
#load "adjacency_mesh.jai";
