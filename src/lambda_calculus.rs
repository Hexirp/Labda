use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct VariableName { pub string: String }

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expression {
    Variable { name: VariableName },
    Application { function_part: Box<Expression>, argument_part: Box<Expression> },
    LambdaAbstraction { bound_variable_name: VariableName, expression: Box<Expression> },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambdaAbstractionExpression { pub bound_variable_name: VariableName, pub expression: Expression }

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CaptureSet { NoCapture, Capture(HashSet<VariableName>) }

impl VariableName {
    pub fn is_variable_in(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Variable { name } =>
                self == name,

            Expression::Application { function_part, argument_part } =>
                self.is_variable_in(function_part) || self.is_variable_in(argument_part),

            Expression::LambdaAbstraction { bound_variable_name, expression } =>
                self == bound_variable_name || self.is_variable_in(expression),
        }
    }

    pub fn is_free_variable_in(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Variable { name } =>
                self == name,

            Expression::Application { function_part, argument_part } =>
                self.is_free_variable_in(function_part) || self.is_free_variable_in(argument_part),

            Expression::LambdaAbstraction { bound_variable_name, expression } =>
                self != bound_variable_name && self.is_free_variable_in(expression),
        }
    }

    pub fn is_bound_variable_in(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Variable { name: _ } =>
                false,

            Expression::Application { function_part, argument_part } =>
                self.is_bound_variable_in(function_part) || self.is_bound_variable_in(argument_part),

            Expression::LambdaAbstraction { bound_variable_name, expression } =>
                self == bound_variable_name || self.is_bound_variable_in(expression),
        }
    }

    pub fn fresh_name(old_name: &VariableName, set: &HashSet<VariableName>) -> VariableName {
        let VariableName { string: old_string } = old_name;

        let mut new_string = old_string.clone();

        while set.contains(&VariableName { string: new_string.clone() }) {
            new_string = new_string + "_";
        }

        VariableName { string: new_string }
    }
}

impl Expression {
    pub fn collect_variable(&self) -> HashSet<VariableName> {
        match self {
            Expression::Variable { name } => {
                let mut set = HashSet::new();

                set.insert(name.clone());

                set
            }

            Expression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_variable());
                set.extend(argument_part.collect_variable());

                set
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_variable());
                set.insert(bound_variable_name.clone());

                set
            }
        }
    }

    pub fn collect_free_variable(&self) -> HashSet<VariableName> {
        match self {
            Expression::Variable { name } => {
                let mut set = HashSet::new();

                set.insert(name.clone());

                set
            }

            Expression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_free_variable());
                set.extend(argument_part.collect_free_variable());

                set
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_free_variable());
                set.remove(bound_variable_name);

                set
            }
        }
    }

    pub fn collect_bound_variable(&self) -> HashSet<VariableName> {
        match self {
            Expression::Variable { name: _ } => {
                let set = HashSet::new();

                set
            }

            Expression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_bound_variable());
                set.extend(argument_part.collect_bound_variable());

                set
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_bound_variable());
                set.insert(bound_variable_name.clone());

                set
            }
        }
    }

    pub fn collect_variable_captured_by(&self, variable_name: &VariableName) -> CaptureSet {
        match self {
            Expression::Variable { name } => {
                if variable_name == name {
                    let set = HashSet::new();

                    CaptureSet::Capture(set)
                } else {
                    CaptureSet::NoCapture
                }
            }

            Expression::Application { function_part, argument_part } => {
                let function_part_result = function_part.collect_variable_captured_by(variable_name);
                let argument_part_result = argument_part.collect_variable_captured_by(variable_name);

                match (function_part_result, argument_part_result) {
                    (CaptureSet::NoCapture, CaptureSet::NoCapture) => {
                        CaptureSet::NoCapture
                    }

                    (CaptureSet::Capture(function_part_result_set), CaptureSet::NoCapture) => {
                        let mut set = HashSet::new();

                        set.extend(function_part_result_set);

                        CaptureSet::Capture(set)
                    }

                    (CaptureSet::NoCapture, CaptureSet::Capture(argument_part_result_set)) => {
                        let mut set = HashSet::new();

                        set.extend(argument_part_result_set);

                        CaptureSet::Capture(set)
                    }

                    (CaptureSet::Capture(function_part_result_set), CaptureSet::Capture(argument_part_result_set)) => {
                        let mut set = HashSet::new();

                        set.extend(function_part_result_set);
                        set.extend(argument_part_result_set);

                        CaptureSet::Capture(set)
                    }
                }
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                if variable_name == bound_variable_name {
                    CaptureSet::NoCapture
                } else {
                    let expression_result = expression.collect_variable_captured_by(variable_name);

                    match expression_result {
                        CaptureSet::NoCapture => {
                            CaptureSet::NoCapture
                        }

                        CaptureSet::Capture(expression_result_set) => {
                            let mut set = HashSet::new();

                            set.extend(expression_result_set);
                            set.insert(bound_variable_name.clone());

                            CaptureSet::Capture(set)
                        }
                    }
                }
            }
        }
    }

    pub fn rename_unsafely(self: Expression, old_variable_name: &VariableName, new_variable_name: &VariableName) -> Expression {
        match self {
            Expression::Variable { name } => {
                if *old_variable_name == name {
                    Expression::Variable { name: new_variable_name.clone() }
                } else {
                    Expression::Variable { name }
                }
            }

            Expression::Application { function_part, argument_part } => {
                let function_part_result = function_part.rename_unsafely(old_variable_name, new_variable_name);
                let argument_part_result = argument_part.rename_unsafely(old_variable_name, new_variable_name);

                Expression::Application { function_part: Box::new(function_part_result), argument_part: Box::new(argument_part_result) }
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                if *old_variable_name == bound_variable_name {
                    Expression::LambdaAbstraction { bound_variable_name, expression }
                } else {
                    let expression_result = expression.rename_unsafely(old_variable_name, new_variable_name);

                    Expression::LambdaAbstraction { bound_variable_name, expression: Box::new(expression_result) }
                }
            }
        }
    }

    pub fn rename(self: Expression, old_variable_name: &VariableName, new_variable_name: &VariableName) -> Option<Expression> {
        let capture_set = self.collect_variable_captured_by(old_variable_name);

        if capture_set.unwrap().contains(new_variable_name) {
            Option::None
        } else {
            Option::Some(self.rename_unsafely(old_variable_name, new_variable_name))
        }
    }

    pub fn substitute(self: Expression, variable_name: &VariableName, right_expression: &Expression) -> Expression {
        match self {
            Expression::Variable { name } => {
                if name == *variable_name {
                    right_expression.clone()
                } else {
                    Expression::Variable { name }
                }
            }

            Expression::Application { function_part, argument_part } => {
                Expression::Application {
                    function_part: Box::new(function_part.substitute(variable_name, right_expression)),
                    argument_part: Box::new(argument_part.substitute(variable_name, right_expression)),
                }
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                if bound_variable_name == *variable_name {
                    Expression::LambdaAbstraction { bound_variable_name, expression }
                } else {
                    let set = {
                        let mut set = HashSet::new();

                        set.extend(expression.collect_variable_captured_by(&bound_variable_name).unwrap());
                        set.extend(right_expression.collect_free_variable());

                        set
                    };

                    let fresh_name = VariableName::fresh_name(&bound_variable_name, &set);

                    Expression::LambdaAbstraction {
                        bound_variable_name: fresh_name.clone(),
                        expression: Box::new(expression
                            .rename(&bound_variable_name, &fresh_name)
                            .unwrap()
                            .substitute(&bound_variable_name, right_expression)),
                    }
                }
            }
        }
    }

    pub fn beta_reduce(function_part: LambdaAbstractionExpression, argument_part: &Expression) -> Expression {
        match function_part {
            LambdaAbstractionExpression { bound_variable_name, expression } => {
                expression.substitute(&bound_variable_name, argument_part)
            }
        }
    }
}

impl LambdaAbstractionExpression {
    pub fn alpha_convert(self: LambdaAbstractionExpression, new_variable_name: &VariableName) -> Option<Expression> {
        let LambdaAbstractionExpression { bound_variable_name, expression } = self;

        expression.rename(&bound_variable_name, new_variable_name)
    }
}

impl CaptureSet {
    pub fn unwrap(self: CaptureSet) -> HashSet<VariableName> {
        match self {
            CaptureSet::NoCapture => {
                HashSet::new()
            }

            CaptureSet::Capture(set) => {
                set
            }
        }
    }
}

#[test]
fn test_x_is_variable_in_y() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };

    assert_eq!(x.is_variable_in(&y), true);
}

#[test]
fn test_x_is_variable_in_z() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };
    let z = Expression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_variable_in(&z), true);
}

#[test]
fn test_x_is_free_variable_in_y() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };

    assert_eq!(x.is_free_variable_in(&y), true);
}

#[test]
fn test_x_is_free_variable_in_z() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };
    let z = Expression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_free_variable_in(&z), false);
}

#[test]
fn test_x_is_bound_variable_in_y() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };

    assert_eq!(x.is_bound_variable_in(&y), false);
}

#[test]
fn test_x_is_bound_variable_in_z() {
    let x = VariableName { string: "a".to_string() };
    let y = Expression::Variable { name: x.clone() };
    let z = Expression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_bound_variable_in(&z), true);
}

#[test]
fn test_w_collect_variable() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_variable(),
        vec![x.clone(), y.clone(), z.clone()].into_iter().collect(),
    );
}

#[test]
fn test_w_collect_free_variable() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_free_variable(),
        vec![x.clone(), z.clone()].into_iter().collect(),
    );
}

#[test]
fn test_w_collect_bound_variable() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_bound_variable(),
        vec![x.clone(), y.clone()].into_iter().collect(),
    );
}

#[test]
fn test_w_collect_variable_captured_by_x() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_variable_captured_by(&x),
        CaptureSet::Capture(vec![].into_iter().collect()),
    );
}

#[test]
fn test_w_collect_variable_captured_by_y() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_variable_captured_by(&y),
        CaptureSet::NoCapture,
    );
}

#[test]
fn test_w_collect_variable_captured_by_z() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        w.collect_variable_captured_by(&z),
        CaptureSet::Capture(vec![x.clone(), y.clone()].into_iter().collect()),
    );
}

#[test]
fn test_v_rename_x_w() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = VariableName { string: "d".to_string() };
    let v = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };
    let u = Expression::Application {
        function_part: Box::new(Expression::Variable { name: w.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        v.rename(&x, &w),
        Option::Some(u),
    )
}

#[test]
fn test_v_rename_y_w() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = VariableName { string: "d".to_string() };
    let v = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };
    let u = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        v.rename(&y, &w),
        Option::Some(u),
    )
}

#[test]
fn test_v_rename_z_w() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let w = VariableName { string: "d".to_string() };
    let v = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };
    let u = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: w.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        v.rename(&z, &w),
        Option::Some(u),
    )
}

#[test]
fn test_v_rename_z_x() {
    let x = VariableName { string: "a".to_string() };
    let y = VariableName { string: "b".to_string() };
    let z = VariableName { string: "c".to_string() };
    let v = Expression::Application {
        function_part: Box::new(Expression::Variable { name: x.clone() }),
        argument_part: Box::new(Expression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(Expression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(Expression::Application {
                    function_part: Box::new(Expression::Variable { name: y.clone() }),
                    argument_part: Box::new(Expression::Variable { name: z.clone() }),
                }),
            }),
        }),
    };

    assert_eq!(
        v.rename(&z, &x),
        Option::None,
    )
}
