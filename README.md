# Scaffold Scripting language for Arbgeom
Still work in progress

## Example:
```
class Plane4D {
    normal: Vec4,
    Proj::proj(vector: Vec4) -> Vec4 {
        vector - normal * dot(vector, normal)
    }
}

// sdihbdsiuchdsc
class Shell {
    offset: f32,
    shape: Any, /* abc */
    Proj::proj(vector: Vec4) -> Vec4 {
        let proj = shape.proj(vector);

        proj + offset * normalize(vector - proj)
    }
}

// Syntax for combining manifolds TBD
class Combo {
    offset: f32,
}
```
