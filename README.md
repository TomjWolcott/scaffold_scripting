# Scaffold Scripting language for Arbgeom
Still a work in progress

## Example:
```
class Sphere4D {
    radius: f32
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
Becomes
```ron
Document {
    classes: [
        Class {
            name: "Sphere4D",
            fields: [
                Binding(
                    "radius",
                    F32,
                ),
            ],
            methods: [],
            instance: None,
        },
        Class {
            name: "Shell",
            fields: [
                Binding(
                    "offset",
                    F32,
                ),
                Binding(
                    "shape",
                    Any,
                ),
            ],
            methods: [
                Method {
                    name: "proj",
                    implementation: Some(
                        "Proj",
                    ),
                    bounds: [
                        Bound {
                            name: "shape",
                            impls: [
                                "Proj",
                            ],
                        },
                    ],
                    inputs: [
                        Binding(
                            "vector",
                            Vec4,
                        ),
                    ],
                    output: Vec4,
                    body: Block(
                        [
                            Declare(
                                Binding(
                                    "proj",
                                    Vec4,
                                ),
                                Dot(
                                    "shape",
                                    "proj",
                                    [
                                        Var(
                                            "vector",
                                        ),
                                    ],
                                ),
                            ),
                        ],
                        Some(
                            BinExpr(
                                Var(
                                    "proj",
                                ),
                                "+",
                                BinExpr(
                                    Var(
                                        "offset",
                                    ),
                                    "*",
                                    Application(
                                        "normalize",
                                        [
                                            BinExpr(
                                                Var(
                                                    "vector",
                                                ),
                                                "-",
                                                Var(
                                                    "proj",
                                                ),
                                            ),
                                        ],
                                    ),
                                ),
                            ),
                        ),
                    ),
                },
            ],
            instance: None,
        },
        Class {
            name: "ShellSphere",
            fields: [
                Binding(
                    "radius",
                    F32,
                ),
            ],
            methods: [],
            instance: Some(
                Instance {
                    name: "Shell",
                    key_vals: [
                        KeyVal {
                            key: "offset",
                            value: Expr(
                                BinExpr(
                                    Lit(
                                        F32(
                                            3.0,
                                        ),
                                    ),
                                    "-",
                                    Var(
                                        "radius",
                                    ),
                                ),
                            ),
                        },
                        KeyVal {
                            key: "shape",
                            value: Instance(
                                Instance {
                                    name: "Sphere4D",
                                    key_vals: [
                                        KeyVal {
                                            key: "radius",
                                            value: Expr(
                                                Var(
                                                    "radius",
                                                ),
                                            ),
                                        },
                                    ],
                                },
                            ),
                        },
                    ],
                },
            ),
        },
    ],
}
```