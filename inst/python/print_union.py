"""Boolean union of prepared millimetre meshes, preserving RGBA properties."""
import argparse
import struct
import sys

import numpy as np
import manifold3d as md


def uint(stream):
    return struct.unpack("<I", stream.read(4))[0]


def doubles(stream, count):
    return np.frombuffer(stream.read(count * 8), dtype="<f8").copy()


def name(stream):
    return stream.read(uint(stream)).decode("utf-8")


def checked(solid, label):
    if solid.status() != md.Error.NoError:
        raise ValueError(f"{label}: Manifold reports {solid.status()}")
    if solid.is_empty():
        raise ValueError(f"{label}: solid is empty")
    return solid


def coloured(solid, rgba):
    return solid.set_properties(4, lambda position, previous: rgba)


def mesh(stream):
    label, role = name(stream), uint(stream)
    nv, nt = uint(stream), uint(stream)
    position = doubles(stream, nv * 3).reshape(nv, 3)
    triangles = np.frombuffer(stream.read(nt * 12), dtype="<u4").astype(np.uint64).reshape(nt, 3)
    rgba = doubles(stream, nt * 4).reshape(nt, 4)
    vertices = triangles.ravel()
    properties = np.column_stack((position[vertices], np.repeat(rgba, 3, axis=0)))
    first = np.full(nv, -1, dtype=np.int64)
    origin, target = [], []
    for i, vertex in enumerate(vertices):
        if first[vertex] < 0:
            first[vertex] = i
        else:
            origin.append(i)
            target.append(first[vertex])
    geometry = md.Mesh64(np.ascontiguousarray(properties), np.arange(nt * 3, dtype=np.uint64).reshape(nt, 3),
                         merge_from_vert=np.array(origin, dtype=np.uint64),
                         merge_to_vert=np.array(target, dtype=np.uint64))
    return label, role, checked(md.Manifold(geometry), label)


def text(stream):
    label, mode = name(stream), uint(stream)
    depth, dilation = doubles(stream, 2)
    rgba = doubles(stream, 4)
    transform = doubles(stream, 12).reshape(3, 4)
    contours = [doubles(stream, uint(stream) * 2).reshape(-1, 2) for _ in range(uint(stream))]
    section = md.CrossSection(contours, md.FillRule.NonZero).offset(dilation, circular_segments=12)
    glyph = md.Manifold.extrude(section, depth * (2 if mode == 2 else 1))
    all_points = np.concatenate(contours)
    low, high = all_points.min(axis=0) - 2 * dilation, all_points.max(axis=0) + 2 * dilation
    border = md.CrossSection([np.array([[low[0], low[1]], [high[0], low[1]],
                                      [high[0], high[1]], [low[0], high[1]]])])
    backing = md.Manifold.extrude(border, depth * (2 if mode == 2 else 1)).translate((0, 0, -depth * (2 if mode == 2 else 1)))
    backing = coloured(backing, np.array([0.85, 0.85, 0.85, 1.0]))
    if mode == 2:
        glyph = glyph.translate((0, 0, -depth))
        solid = backing - coloured(glyph, rgba)
    else:
        solid = backing + coloured(glyph, rgba)
    return label, checked(solid.transform(transform), label)


def run(source, destination):
    groups, engravers, labels = {}, [], []
    with open(source, "rb") as stream:
        if stream.read(8) != b"GGPRINT1":
            raise ValueError("invalid printing mesh stream")
        meshes, texts = uint(stream), uint(stream)
        for _ in range(meshes):
            label, role, solid = mesh(stream)
            labels.append(label)
            if role == 2:
                engravers.append(solid)
            else:
                group = groups.setdefault(label, [[], []])
                group[role].append(solid)
        positives = []
        for label, (adds, holes) in groups.items():
            solid = checked(md.Manifold.batch_boolean(adds, md.OpType.Add), label)
            if holes:
                cavity = checked(md.Manifold.batch_boolean(holes, md.OpType.Add), label)
                if not (cavity - solid).is_empty():
                    raise ValueError(f"{label}: negative-winding cavity extends outside its solid")
                solid = checked(solid - cavity, label)
            positives.append(solid)
        for _ in range(texts):
            label, solid = text(stream)
            labels.append(label)
            positives.append(solid)
    result = checked(md.Manifold.batch_boolean(positives, md.OpType.Add), ", ".join(dict.fromkeys(labels)))
    if engravers:
        result = checked(result - md.Manifold.batch_boolean(engravers, md.OpType.Add), ", ".join(dict.fromkeys(labels)))
    bounds = result.bounding_box()
    result = result.translate(tuple(-v for v in bounds[:3]))
    output = result.to_mesh64()
    properties = np.asarray(output.vert_properties, dtype="<f8")
    triangles = np.asarray(output.tri_verts, dtype="<u4")
    with open(destination, "wb") as stream:
        stream.write(b"GGUNION1")
        stream.write(struct.pack("<III", len(properties), len(triangles), properties.shape[1]))
        stream.write(properties.tobytes(order="C"))
        stream.write(triangles.tobytes(order="C"))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("source")
    parser.add_argument("destination")
    args = parser.parse_args()
    try:
        run(args.source, args.destination)
    except Exception as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
