#import "Math";
#import "Basic";

#import "turns";
#import "linear_algebra";
#import "math";


Indexed_Triangle_Positions :: struct {
    indices : [..] u32; 
    positions : [..] Vector3; 
}

generate_annulus :: (center: Vector3, normal: Vector3, inner_radius: float, outer_radius: float, num_segments: int) -> Indexed_Triangle_Positions {
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

        // the idea is that if you cut this as a strip, you'd just be making quads
        array_add(*annulus.indices, cast (u32)(2 * it));
        array_add(*annulus.indices, cast (u32)(2 * next));
        array_add(*annulus.indices, cast (u32)(2 * it + 1));

        array_add(*annulus.indices, cast (u32)(2 * next));
        array_add(*annulus.indices, cast (u32)(2 * next + 1));
        array_add(*annulus.indices, cast (u32)(2 * it + 1));
    }

    return annulus;
}

