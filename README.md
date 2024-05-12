# Scripting Language for Arbgeom: Structured Scaffolding language
Still a work in progress

## Scripting file:
Contains the interfaces and classes to be used.  You can see the defined functions [here](src/interpreter.rs?plain=1#L332)
```
// Might be defined in the file, might be defined by the code executing the scripts
interface Sdf {
    sdf(vector: Vec4) -> f32,
}

interface Proj {
    proj(vector: Vec4) -> Vec4,
}

class Plane4D {
    normal: Vec4,
    Proj::proj(vector: Vec4) -> Vec4 {
        vector - normal * dot(vector, normal)
    },
    Sdf::sdf(vector: Vec4) -> f32 {
        dot(vector, normal)
    }
}

class Sphere4D {
    radius: f32,
    Proj::proj(vector: Vec4) -> Vec4 {
        radius * normalize(vector)
    },
    Sdf::sdf(vector: Vec4) -> f32 {
        length(vector) - radius
    }
}

class Shell {
    offset: f32,
    shape: Class,
    Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
        let proj = shape.proj(vector);

        proj + offset * normalize(vector - proj)
    },
    Sdf::sdf<shape: Sdf>(vector: Vec4) -> f32 {
        abs(shape.sdf(vector)) - offset
    }
}

// This is an instanced class
class DoubleShell {
    offset: f32,
    other_shape: Class
} => Shell {
    offset,
    shape: Shell {
        offset: offset / 2,
        shape: other_shape
    }
}
```

These structures can then be combined.  This is done by [RON](https://github.com/ron-rs/ron):
```ron
Shell(
    offset: Expr("value - 2"),
    shape: Plane4D( normal: [1, 0, 0, 0] )
)
```
which is immediately preprocessed to:
```ron
(__struct_name: "Shell",
    offset: (__struct_name: "Expr", expr: "value - 2"),
    shape: (__struct_name: "Plane4D", normal: [1, 0, 0, 0] )
)
```

You can also just directly create instances like any other class:
```ron
DoubleShell(
    offset: 17.3,
    shape: Sphere4D( radius: Expr("min(abc, 3)") )
)
```

These are assembled into Assembled structures which hold methods 
that can be interpreted or directly transpiled to WGSL and fields 
that need to be evaluated and then can be used.  If compiling to WGSL
you will need to make you're own system for moving the fields as data
arrays.  Use ident_scope on to_wgsl to do this.