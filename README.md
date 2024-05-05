# Scaffold Scripting language for Arbgeom
Still a work in progress

## Example:
```
class Sphere4D {
    radius: f32,
    test(abc: f32, xyz: Mat4) -> bool {
        // (((abc - 3) - (((2 / 8) / 3) * 2)) + 1)
        abc - 3 - -2 / -8 / 3 * 2 + (-1) + !27 + Vec4(100, -4, 3 - x, .2)
    }
}

class Shell {
    offset: f32,
    shape: Any,
    Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
        let proj: Vec4 = shape.proj(vector);

        proj + offset * normalize(vector - proj)
    }
}

class ShellSphere {
    radius: f32
} => Shell {
    offset: 3.0 - radius,
    shape: Sphere4D {
        radius
    }
}
```
