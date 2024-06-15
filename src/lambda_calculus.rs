use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct VariableName { pub string: String }

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

impl VariableName {
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

impl VariableName {
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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expression {
    Variable { name: VariableName },
    Application { function_part: Box<Expression>, argument_part: Box<Expression> },
    LambdaAbstraction { bound_variable_name: VariableName, expression: Box<Expression> },
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
        HashSet::from([x.clone(), y.clone(), z.clone()]),
    );
}

impl Expression {
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
        HashSet::from([x.clone(), z.clone()]),
    );
}

impl Expression {
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
        HashSet::from([x.clone(), y.clone()]),
    );
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CaptureSet { NoCapture, Capture(HashSet<VariableName>) }

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

impl Expression {
    pub fn collect_variable_captured_by(&self, variable_name: &VariableName) -> CaptureSet {
        match self {
            Expression::Variable { name } => {
                if name == variable_name {
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
                if bound_variable_name == variable_name {
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
        CaptureSet::Capture(HashSet::from([])),
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
        CaptureSet::Capture(HashSet::from([x.clone(), y.clone()])),
    );
}

impl Expression {
    pub fn rename_unsafely(self: Expression, old_variable_name: &VariableName, new_variable_name: &VariableName) -> Expression {
        match self {
            Expression::Variable { name } => {
                if &name == old_variable_name {
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
                if &bound_variable_name == old_variable_name {
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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambdaAbstractionExpression { pub bound_variable_name: VariableName, pub expression: Expression }

impl LambdaAbstractionExpression {
    pub fn alpha_convert(self: LambdaAbstractionExpression, new_variable_name: &VariableName) -> Option<Expression> {
        let LambdaAbstractionExpression { bound_variable_name, expression } = self;

        expression.rename(&bound_variable_name, new_variable_name)
    }
}

impl VariableName {
    pub fn fresh(self: &VariableName, set: &HashSet<VariableName>) -> VariableName {
        let mut new_name = self.clone();

        while set.contains(&new_name) {
            new_name.string = new_name.string + "_";
        }

        new_name
    }
}

impl Expression {
    pub fn substitute(self: Expression, variable_name: &VariableName, right_expression: Expression) -> Expression {
        match self {
            Expression::Variable { name } => {
                if &name == variable_name {
                    right_expression
                } else {
                    Expression::Variable { name }
                }
            }

            Expression::Application { function_part, argument_part } => {
                Expression::Application {
                    function_part: Box::new(function_part.substitute(variable_name, right_expression.clone())),
                    argument_part: Box::new(argument_part.substitute(variable_name, right_expression.clone())),
                }
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                if &bound_variable_name == variable_name {
                    Expression::LambdaAbstraction { bound_variable_name, expression }
                } else {
                    let mut set = HashSet::new();

                    set.extend(expression.collect_variable_captured_by(&bound_variable_name).unwrap());
                    set.extend(right_expression.collect_free_variable());

                    let fresh_name = bound_variable_name.fresh(&set);

                    let old_expression = LambdaAbstractionExpression { bound_variable_name, expression: *expression };

                    let new_expression = old_expression
                        .alpha_convert(&fresh_name)
                        .unwrap()
                        .substitute(&fresh_name, right_expression);

                    Expression::LambdaAbstraction {
                        bound_variable_name: fresh_name,
                        expression: Box::new(new_expression),
                    }
                }
            }
        }
    }
}

impl LambdaAbstractionExpression {
    pub fn beta_reduce(self: LambdaAbstractionExpression, argument_part: Expression) -> Expression {
        let LambdaAbstractionExpression { bound_variable_name, expression } = self;

        expression.substitute(&bound_variable_name, argument_part)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ReductionResult { NormalForm { expression: Expression }, Reduced { expression: Expression } }

impl Expression {
    pub fn reduce(self: Expression) -> ReductionResult {
        match self {
            Expression::Variable { name } => {
                ReductionResult::NormalForm { expression: Expression::Variable { name } }
            }

            Expression::Application { function_part, argument_part } => {
                match *function_part {
                    Expression::Variable { name } => {
                        match argument_part.reduce() {
                            ReductionResult::NormalForm { expression } => {
                                ReductionResult::NormalForm {
                                    expression: Expression::Application {
                                        function_part: Box::new(Expression::Variable { name }),
                                        argument_part: Box::new(expression),
                                    }
                                }
                            }

                            ReductionResult::Reduced { expression } => {
                                ReductionResult::Reduced {
                                    expression: Expression::Application {
                                        function_part: Box::new(Expression::Variable { name }),
                                        argument_part: Box::new(expression),
                                    }
                                }
                            }
                        }
                    }

                    Expression::Application { function_part, argument_part } => {
                        match function_part.reduce() {
                            ReductionResult::NormalForm { expression: function_part_result } => {
                                match argument_part.reduce() {
                                    ReductionResult::NormalForm { expression: argument_part_result } => {
                                        ReductionResult::NormalForm {
                                            expression: Expression::Application {
                                                function_part: Box::new(function_part_result),
                                                argument_part: Box::new(argument_part_result),
                                            }
                                        }
                                    }

                                    ReductionResult::Reduced { expression: argument_part_result } => {
                                        ReductionResult::Reduced {
                                            expression: Expression::Application {
                                                function_part: Box::new(function_part_result),
                                                argument_part: Box::new(argument_part_result),
                                            }
                                        }
                                    }
                                }
                            }

                            ReductionResult::Reduced { expression: function_part_result } => {
                                ReductionResult::Reduced {
                                    expression: Expression::Application {
                                        function_part: Box::new(function_part_result),
                                        argument_part,
                                    }
                                }
                            }
                        }
                    }

                    Expression::LambdaAbstraction { bound_variable_name, expression } => {
                        let function_part = LambdaAbstractionExpression { bound_variable_name, expression: *expression };

                        ReductionResult::Reduced { expression: function_part.beta_reduce(*argument_part) }
                    }
                }
            }

            Expression::LambdaAbstraction { bound_variable_name, expression } => {
                match expression.reduce() {
                    ReductionResult::NormalForm { expression } => {
                        ReductionResult::NormalForm {
                            expression: Expression::LambdaAbstraction {
                                bound_variable_name,
                                expression: Box::new(expression),
                            }
                        }
                    }

                    ReductionResult::Reduced { expression } => {
                        ReductionResult::Reduced {
                            expression: Expression::LambdaAbstraction {
                                bound_variable_name,
                                expression: Box::new(expression),
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Time { number: u64 }

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum TimeLimit {
    Finite { time: Time },
    Infinite,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum EvaluationResult {
    Completed { expression: Expression, time: Time },
    Timeout { expression: Expression },
}

impl Expression {
    pub fn evaluate(self: Expression, time_limit: TimeLimit) -> EvaluationResult {
        let mut expression = self;
        let mut time = Time { number: 0 };
        let mut yet = true;

        while (yet || TimeLimit::Finite { time } < time_limit) {
            match expression.reduce() {
                ReductionResult::NormalForm { expression: result } => {
                    expression = result;
                }

                ReductionResult::Reduced { expression: result } => {
                    expression = result;
                    yet = false;
                }
            }

            time.number = time.number + 1;
        }

        if yet {
            EvaluationResult::Timeout { expression }
        } else {
            EvaluationResult::Completed { expression, time }
        }
    }
}
