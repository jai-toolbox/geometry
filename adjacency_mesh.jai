/*

This is the "half-edge" datastructure instead we call it an adjacency_mesh because at least that tells you somewhat that it's encoding adjacency into the mesh

This was created without caring about memory yet, when that's an actual concern I think we'll use a pool allocator with a free list or something.

*/

#import "Hash_Table";


Edge :: struct {
    same_edge_other_face:  *Edge;
    next_edge_around_face: *Edge;
    prev_edge_around_face: *Edge;
    start_vertex:          *Vertex;
    left_face:             *Face;
}

Vertex :: struct {
    position:                Vector3;
    arbitrary_outgoing_edge: *Edge;
}

Face :: struct {
    arbitrary_edge: *Edge;
}

Adjacency_Mesh :: struct {
    vertices: [..] *Vertex;
    edges:    [..] *Edge;
    faces:    [..] *Face;
}

make_vertex :: (mesh: *Adjacency_Mesh, pos: Vector3) -> *Vertex {
    v := New(Vertex);
    v.position = pos;
    array_add(*mesh.vertices, v);
    return v;
}

make_edge :: (mesh: *Adjacency_Mesh) -> *Edge {
    e := New(Edge);
    array_add(*mesh.edges, e);
    return e;
}

make_face :: (mesh: *Adjacency_Mesh) -> *Face {
    f := New(Face);
    array_add(*mesh.faces, f);
    return f;
}

destroy_mesh :: (mesh: *Adjacency_Mesh) {
    for mesh.vertices  free(it);
    for mesh.edges     free(it);
    for mesh.faces     free(it);
    array_reset(*mesh.vertices);
    array_reset(*mesh.edges);
    array_reset(*mesh.faces);
}


make_face_from_vertices :: (mesh: *Adjacency_Mesh, verts: [] *Vertex) -> *Face {
    assert(verts.count >= 3);

    f := make_face(mesh);
    n := verts.count;

    new_edges: [..] *Edge;
    defer array_reset(*new_edges);

    for 0..n-1 {
        e := make_edge(mesh);
        e.start_vertex = verts[it];
        e.left_face    = f;
        if !verts[it].arbitrary_outgoing_edge  verts[it].arbitrary_outgoing_edge = e;
        array_add(*new_edges, e);
    }

    for 0..n-1 {
        new_edges[it].next_edge_around_face = new_edges[(it + 1) % n];
        new_edges[it].prev_edge_around_face = new_edges[(it + n - 1) % n];
    }

    f.arbitrary_edge = new_edges[0];
    return f;
}

link_shared_edges :: (mesh: *Adjacency_Mesh) {
    Edge_Key :: struct {
        a: *Vertex;
        b: *Vertex;
    }

    edge_key_hash :: (key: Edge_Key) -> u32 {
        h1 := cast,no_check(u64) key.a;
        h2 := cast,no_check(u64) key.b;
        combined := h1 * 0x9e3779b97f4a7c15 + h2;
        return cast,no_check(u32) combined;
    }

    edge_key_compare :: (a: Edge_Key, b: Edge_Key) -> bool {
        return a.a == b.a && a.b == b.b;
    }

    edge_map: Table(Edge_Key, *Edge, edge_key_hash, edge_key_compare);
    defer deinit(*edge_map);

    for mesh.edges {
        a := it.start_vertex;
        b := it.next_edge_around_face.start_vertex;

        reverse := Edge_Key.{b, a};
        other_face_edge, found := table_find(*edge_map, reverse);

        if found {
            it.same_edge_other_face               = other_face_edge;
            other_face_edge.same_edge_other_face  = it;
            table_remove(*edge_map, reverse);
        } else {
            table_set(*edge_map, Edge_Key.{a, b}, it);
        }
    }
}


make_quad :: (mesh: *Adjacency_Mesh, p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3) -> *Face {
    v0 := make_vertex(mesh, p0);
    v1 := make_vertex(mesh, p1);
    v2 := make_vertex(mesh, p2);
    v3 := make_vertex(mesh, p3);

    f := make_face_from_vertices(mesh, *Vertex.[v0, v1, v2, v3]);
    link_shared_edges(mesh);
    return f;
}

make_cube :: (mesh: *Adjacency_Mesh, size: float) -> *Adjacency_Mesh {
    s := size * 0.5;

    v0 := make_vertex(mesh, .{-s, -s, -s});
    v1 := make_vertex(mesh, .{ s, -s, -s});
    v2 := make_vertex(mesh, .{ s, -s,  s});
    v3 := make_vertex(mesh, .{-s, -s,  s});
    v4 := make_vertex(mesh, .{-s,  s, -s});
    v5 := make_vertex(mesh, .{ s,  s, -s});
    v6 := make_vertex(mesh, .{ s,  s,  s});
    v7 := make_vertex(mesh, .{-s,  s,  s});

    make_face_from_vertices(mesh, *Vertex.[v0, v3, v2, v1]); // bottom
    make_face_from_vertices(mesh, *Vertex.[v4, v5, v6, v7]); // top
    make_face_from_vertices(mesh, *Vertex.[v0, v1, v5, v4]); // front
    make_face_from_vertices(mesh, *Vertex.[v2, v3, v7, v6]); // back
    make_face_from_vertices(mesh, *Vertex.[v0, v4, v7, v3]); // left
    make_face_from_vertices(mesh, *Vertex.[v1, v2, v6, v5]); // right

    link_shared_edges(mesh);
    return mesh;
}

face_vertices :: (face: *Face) -> [..] *Vertex {
    result: [..] *Vertex;
    edge := face.arbitrary_edge;
    while true {
        array_add(*result, edge.start_vertex);
        edge = edge.next_edge_around_face;
        if edge == face.arbitrary_edge break;
    }
    return result;
}

vertex_faces :: (v: *Vertex) -> [..] *Face {
    result: [..] *Face;
    edge := v.arbitrary_outgoing_edge;
    if !edge return result;

    start := edge;
    while true {
        if edge.left_face  array_add(*result, edge.left_face);
        edge = edge.prev_edge_around_face.same_edge_other_face;
        if !edge  break;
        if edge == start break;
    }
    return result;
}

face_normal :: (face: *Face) -> Vector3 {
    normal := Vector3.{0,0,0};
    edge := face.arbitrary_edge;
    while true {
        a := edge.start_vertex.position;
        b := edge.next_edge_around_face.start_vertex.position;
        normal.x += (a.y - b.y) * (a.z + b.z);
        normal.y += (a.z - b.z) * (a.x + b.x);
        normal.z += (a.x - b.x) * (a.y + b.y);
        edge = edge.next_edge_around_face;
        if edge == face.arbitrary_edge break;
    }
    return unit_vector(normal);
}


extrude_face :: (mesh: *Adjacency_Mesh, face: *Face, offset: float) {
    normal := face_normal(face);

    // determine if this face is entirely on the boundary
    is_boundary_face := true;
    {
        edge := face.arbitrary_edge;
        while true {
            if edge.same_edge_other_face != null {
                is_boundary_face = false;
                break;
            }
            edge = edge.next_edge_around_face;
            if edge == face.arbitrary_edge break;
        }
    }

    Boundary_Info :: struct {
        edge:       *Edge;
        old_vertex: *Vertex;
        new_vertex: *Vertex;
    }

    boundary: [..] Boundary_Info;
    defer array_reset(*boundary);

    edge := face.arbitrary_edge;
    while true {
        new_v := make_vertex(mesh, edge.start_vertex.position + normal * offset);
        array_add(*boundary, .{edge, edge.start_vertex, new_v});
        edge = edge.next_edge_around_face;
        if edge == face.arbitrary_edge break;
    }

    n := boundary.count;

    if is_boundary_face {
        for 0..n-1 {
            j := (it + 1) % n;
            make_face_from_vertices(mesh, .[
                boundary[it].old_vertex,
                boundary[j].old_vertex,
                boundary[j].new_vertex,
                boundary[it].new_vertex
            ]);
        }

        top_verts: [..] *Vertex;
        top_verts.allocator = temp;
        for 0..n-1 {
            array_add(*top_verts, boundary[it].new_vertex);
        }
        make_face_from_vertices(mesh, top_verts);

    } else {
        for 0..n-1 {
            j := (it + 1) % n;
            make_face_from_vertices(mesh, .[
                boundary[it].old_vertex,
                boundary[j].old_vertex,
                boundary[j].new_vertex,
                boundary[it].new_vertex
            ]);
        }

        for 0..n-1 {
            boundary[it].edge.start_vertex = boundary[it].new_vertex;
            boundary[it].new_vertex.arbitrary_outgoing_edge = boundary[it].edge;
        }
    }

    for mesh.edges {
        it.same_edge_other_face = null;
    }
    link_shared_edges(mesh);
}

inset_face :: (mesh: *Adjacency_Mesh, face: *Face, inset_amount: float) {
    normal := face_normal(face);

    // collect boundary info: the edges and vertices of the original face
    Boundary_Info :: struct {
        edge:       *Edge;
        old_vertex: *Vertex;
        new_vertex: *Vertex;
    }

    boundary: [..] Boundary_Info;
    defer array_reset(*boundary);

    centroid := Vector3.{0, 0, 0};
    vert_count := 0;
    {
        edge := face.arbitrary_edge;
        while true {
            centroid += edge.start_vertex.position;
            vert_count += 1;
            edge = edge.next_edge_around_face;
            if edge == face.arbitrary_edge break;
        }
    }
    centroid = centroid * (1.0 / cast(float) vert_count);

    // create new inset vertices (moved toward centroid by inset_amount, clamped 0..1)
    t := clamp(inset_amount, 0.0, 1.0);
    {
        edge := face.arbitrary_edge;
        while true {
            inset_pos := edge.start_vertex.position + (centroid - edge.start_vertex.position) * t;
            new_v := make_vertex(mesh, inset_pos);
            array_add(*boundary, .{edge, edge.start_vertex, new_v});
            edge = edge.next_edge_around_face;
            if edge == face.arbitrary_edge break;
        }
    }

    n := boundary.count;

    // create connecting quads between outer and inner vertices.
    // each quad connects: old[i] -> old[i+1] -> new[i+1] -> new[i]
    for 0..n-1 {
        j := (it + 1) % n;
        make_face_from_vertices(mesh, .[
            boundary[it].old_vertex,
            boundary[j].old_vertex,
            boundary[j].new_vertex,
            boundary[it].new_vertex
        ]);
    }

    // reassign the original face's edges to use the new (inset) vertices.
    // this makes the original face become the inner face.
    for 0..n-1 {
        boundary[it].edge.start_vertex = boundary[it].new_vertex;
        boundary[it].new_vertex.arbitrary_outgoing_edge = boundary[it].edge;
    }

    // re-link all twin pointers
    for mesh.edges {
        it.same_edge_other_face = null;
    }
    link_shared_edges(mesh);
}


split_edge :: (mesh: *Adjacency_Mesh, edge: *Edge) -> *Vertex {
    other_face_edge := edge.same_edge_other_face;

    a := edge.start_vertex;
    b := edge.next_edge_around_face.start_vertex;
    mid_pos := (a.position + b.position) * 0.5;
    mid := make_vertex(mesh, mid_pos);

    new_edge := make_edge(mesh);
    new_edge.start_vertex          = mid;
    new_edge.left_face             = edge.left_face;
    new_edge.next_edge_around_face = edge.next_edge_around_face;
    new_edge.prev_edge_around_face = edge;
    edge.next_edge_around_face.prev_edge_around_face = new_edge;
    edge.next_edge_around_face     = new_edge;

    mid.arbitrary_outgoing_edge = new_edge;

    if other_face_edge {
        new_other := make_edge(mesh);
        new_other.start_vertex          = mid;
        new_other.left_face             = other_face_edge.left_face;
        new_other.next_edge_around_face = other_face_edge.next_edge_around_face;
        new_other.prev_edge_around_face = other_face_edge;
        other_face_edge.next_edge_around_face.prev_edge_around_face = new_other;
        other_face_edge.next_edge_around_face = new_other;

        edge.same_edge_other_face       = new_other;
        new_other.same_edge_other_face  = edge;
        new_edge.same_edge_other_face   = other_face_edge;
        other_face_edge.same_edge_other_face = new_edge;
    }

    return mid;
}

remove_from_array :: (arr: *[..] $T, item: T) {
    for arr.* {
        if it == item {
            arr.*[it_index] = arr.*[arr.count - 1];
            arr.count -= 1;
            return;
        }
    }
}

remove_face :: (mesh: *Adjacency_Mesh, face: *Face) {
    edges_to_remove: [..] *Edge;
    defer array_reset(*edges_to_remove);

    edge := face.arbitrary_edge;
    while true {
        array_add(*edges_to_remove, edge);
        edge = edge.next_edge_around_face;
        if edge == face.arbitrary_edge break;
    }

    for edges_to_remove {
        if it.same_edge_other_face  it.same_edge_other_face.same_edge_other_face = null;

        if it.start_vertex.arbitrary_outgoing_edge == it {
            if it.same_edge_other_face && it.same_edge_other_face.next_edge_around_face {
                it.start_vertex.arbitrary_outgoing_edge = it.same_edge_other_face.next_edge_around_face;
            } else {
                it.start_vertex.arbitrary_outgoing_edge = null;
            }
        }

        remove_from_array(*mesh.edges, it);
        free(it);
    }

    remove_from_array(*mesh.faces, face);
    free(face);
}


// itp / itpn conversion

make_adjacency_mesh_from_itp :: (itp: *Indexed_Triangle_Positions) -> Adjacency_Mesh {
    mesh: Adjacency_Mesh;

    // create one vertex per position entry
    vertex_map: [..] *Vertex;
    defer array_reset(*vertex_map);
    array_reserve(*vertex_map, itp.positions.count);

    for 0..itp.positions.count-1 {
        v := make_vertex(*mesh, itp.positions[it]);
        array_add(*vertex_map, v);
    }

    // walk triangles and build faces
    assert(itp.indices.count % 3 == 0);
    tri_count := itp.indices.count / 3;

    for 0..tri_count-1 {
        i0 := itp.indices[it * 3 + 0];
        i1 := itp.indices[it * 3 + 1];
        i2 := itp.indices[it * 3 + 2];

        make_face_from_vertices(*mesh, *Vertex.[
            vertex_map[i0],
            vertex_map[i1],
            vertex_map[i2]
        ]);
    }

    // link twin half-edges across shared edges
    link_shared_edges(*mesh);

    return mesh;
}


make_itp_from_adjacency_mesh :: (adj: *Adjacency_Mesh) -> Indexed_Triangle_Positions {
    result: Indexed_Triangle_Positions;

    vertex_index_map: Table(*Vertex, u32);
    defer deinit(*vertex_index_map);

    get_or_add :: (result: *Indexed_Triangle_Positions, map: *Table(*Vertex, u32), v: *Vertex) -> u32 {
        idx, found := table_find(map, v);
        if found return idx;
        idx = cast(u32) result.positions.count;
        array_add(*result.positions, v.position);
        table_set(map, v, idx);
        return idx;
    }

    for adj.faces {
        face_indices: [..] u32;
        defer array_reset(*face_indices);

        edge := it.arbitrary_edge;
        while true {
            idx := get_or_add(*result, *vertex_index_map, edge.start_vertex);
            array_add(*face_indices, idx);
            edge = edge.next_edge_around_face;
            if edge == it.arbitrary_edge break;
        }

        // fan triangulation for n-gons, could make this a parameter later.
        for 1..face_indices.count - 2 {
            array_add(*result.indices, face_indices[0]);
            array_add(*result.indices, face_indices[it]);
            array_add(*result.indices, face_indices[it + 1]);
        }
    }

    return result;
}

make_itpn_from_adjacency_mesh :: (adj: *Adjacency_Mesh) -> Indexed_Triangle_Position_Normals {
    result: Indexed_Triangle_Position_Normals;

    vertex_index_map: Table(*Vertex, u32);
    defer deinit(*vertex_index_map);

    get_or_add :: (result: *Indexed_Triangle_Position_Normals, map: *Table(*Vertex, u32), v: *Vertex) -> u32 {
        idx, found := table_find(map, v);
        if found return idx;
        idx = cast(u32) result.positions.count;
        array_add(*result.positions, v.position);
        array_add(*result.normals, .{0, 0, 0});
        table_set(map, v, idx);
        return idx;
    }

    for adj.faces {
        normal := face_normal(it);

        face_indices: [..] u32;
        defer array_reset(*face_indices);

        edge := it.arbitrary_edge;
        while true {
            idx := get_or_add(*result, *vertex_index_map, edge.start_vertex);
            array_add(*face_indices, idx);
            result.normals[idx] += normal;
            edge = edge.next_edge_around_face;
            if edge == it.arbitrary_edge break;
        }

        for 1..face_indices.count - 2 {
            array_add(*result.indices, face_indices[0]);
            array_add(*result.indices, face_indices[it]);
            array_add(*result.indices, face_indices[it + 1]);
        }
    }

    for *n : result.normals {
        n.* = unit_vector(n.*);
    }

    return result;
}


validate_mesh :: (mesh: *Adjacency_Mesh) -> bool {
    ok := true;

    for mesh.edges {
        if !it.next_edge_around_face { log_error("edge has null next_edge_around_face"); ok = false; continue; }
        if !it.prev_edge_around_face { log_error("edge has null prev_edge_around_face"); ok = false; continue; }
        if !it.start_vertex          { log_error("edge has null start_vertex");          ok = false; continue; }

        if it.next_edge_around_face.prev_edge_around_face != it {
            log_error("next.prev != self");
            ok = false;
        }

        if it.prev_edge_around_face.next_edge_around_face != it {
            log_error("prev.next != self");
            ok = false;
        }

        if it.same_edge_other_face {
            if it.same_edge_other_face.same_edge_other_face != it {
                log_error("same_edge_other_face is not symmetric");
                ok = false;
            }

            if it.same_edge_other_face.start_vertex != it.next_edge_around_face.start_vertex {
                log_error("same_edge_other_face.start_vertex should equal next.start_vertex");
                ok = false;
            }

            if it.same_edge_other_face.next_edge_around_face.start_vertex != it.start_vertex {
                log_error("same_edge_other_face.next.start_vertex should equal start_vertex");
                ok = false;
            }
        }

        if it.left_face {
            if it.next_edge_around_face.left_face != it.left_face {
                log_error("next.left_face != left_face");
                ok = false;
            }
        }
    }

    for mesh.faces {
        edge := it.arbitrary_edge;
        count := 0;
        while true {
            count += 1;
            if count > mesh.edges.count {
                log_error("face loop does not close!");
                ok = false;
                break;
            }
            edge = edge.next_edge_around_face;
            if edge == it.arbitrary_edge break;
        }
    }

    if ok  log("mesh validation passed: % vertices, % edges, % faces",
               mesh.vertices.count, mesh.edges.count, mesh.faces.count);

    return ok;
}
