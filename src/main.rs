use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct LambdaCalculusVariableName { string: String }

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum LambdaCalculusExpression {
    Variable { name: LambdaCalculusVariableName },
    Application { function_part: Box<LambdaCalculusExpression>, argument_part: Box<LambdaCalculusExpression> },
    LambdaAbstraction { bound_variable_name: LambdaCalculusVariableName, expression: Box<LambdaCalculusExpression> },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct LambdaCalculusLambdaAbstractionExpression { bound_variable_name: LambdaCalculusVariableName, expression: Box<LambdaCalculusExpression> }

impl LambdaCalculusVariableName {
    fn is_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name } =>
                self == name,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_variable_in(function_part) || self.is_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } =>
                self == bound_variable_name || self.is_variable_in(expression),
        }
    }

    fn is_free_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name } =>
                self == name,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_free_variable_in(function_part) || self.is_free_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } =>
                self != bound_variable_name && self.is_free_variable_in(expression),
        }
    }

    fn is_bound_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name: _ } =>
                false,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_bound_variable_in(function_part) || self.is_bound_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } =>
                self == bound_variable_name || self.is_bound_variable_in(expression),
        }
    }
}

impl LambdaCalculusExpression {
    fn collect_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        match self {
            LambdaCalculusExpression::Variable { name } => {
                let mut set = HashSet::new();

                set.insert(name.clone());

                set
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_variable());
                set.extend(argument_part.collect_variable());

                set
            }

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_variable());
                set.insert(bound_variable_name.clone());

                set
            }
        }
    }

    fn collect_free_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        match self {
            LambdaCalculusExpression::Variable { name } => {
                let mut set = HashSet::new();

                set.insert(name.clone());

                set
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_free_variable());
                set.extend(argument_part.collect_free_variable());

                set
            }

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_free_variable());
                set.remove(bound_variable_name);

                set
            }
        }
    }

    fn collect_bound_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        match self {
            LambdaCalculusExpression::Variable { name: _ } => {
                let set = HashSet::new();

                set
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                let mut set = HashSet::new();

                set.extend(function_part.collect_bound_variable());
                set.extend(argument_part.collect_bound_variable());

                set
            }

            LambdaCalculusExpression::LambdaAbstraction { bound_variable_name, expression } => {
                let mut set = HashSet::new();

                set.extend(expression.collect_variable());
                set.insert(bound_variable_name.clone());

                set
            }
        }
    }

    fn beta_reduce(function_part: &LambdaCalculusLambdaAbstractionExpression, argument_part: &LambdaCalculusExpression) -> LambdaCalculusExpression {
        match function_part {
            LambdaCalculusLambdaAbstractionExpression { bound_variable_name: _, expression: _ } => {
                todo!();
            }
        }
    }
}

#[test]
fn test_x_is_variable_in_y() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };

    assert_eq!(x.is_variable_in(&y), true);
}

#[test]
fn test_x_is_variable_in_z() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };
    let z = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_variable_in(&z), true);
}

#[test]
fn test_x_is_free_variable_in_y() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };

    assert_eq!(x.is_free_variable_in(&y), true);
}

#[test]
fn test_x_is_free_variable_in_z() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };
    let z = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_free_variable_in(&z), false);
}

#[test]
fn test_x_is_bound_variable_in_y() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };

    assert_eq!(x.is_bound_variable_in(&y), false);
}

#[test]
fn test_x_is_bound_variable_in_z() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusExpression::Variable { name: x.clone() };
    let z = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

    assert_eq!(x.is_bound_variable_in(&z), true);
}

#[test]
fn test_z_collect_variable() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusVariableName { string: "b".to_string() };
    let z = LambdaCalculusExpression::Application {
        function_part: Box::new(LambdaCalculusExpression::Variable { name: x.clone() }),
        argument_part: Box::new(LambdaCalculusExpression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(LambdaCalculusExpression::Variable { name: y.clone()})
            })
        }),
    };

    assert_eq!(
        z.collect_variable(),
        vec![LambdaCalculusVariableName { string: "a".to_string() }, LambdaCalculusVariableName { string: "b".to_string() }].into_iter().collect(),
    );
}

#[test]
fn test_z_collect_free_variable() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusVariableName { string: "b".to_string() };
    let z = LambdaCalculusExpression::Application {
        function_part: Box::new(LambdaCalculusExpression::Variable { name: x.clone() }),
        argument_part: Box::new(LambdaCalculusExpression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(LambdaCalculusExpression::Variable { name: y.clone()})
            })
        }),
    };

    assert_eq!(
        z.collect_free_variable(),
        vec![LambdaCalculusVariableName { string: "a".to_string() }].into_iter().collect(),
    );
}

#[test]
fn test_z_collect_bound_variable() {
    let x = LambdaCalculusVariableName { string: "a".to_string() };
    let y = LambdaCalculusVariableName { string: "b".to_string() };
    let z = LambdaCalculusExpression::Application {
        function_part: Box::new(LambdaCalculusExpression::Variable { name: x.clone() }),
        argument_part: Box::new(LambdaCalculusExpression::LambdaAbstraction {
            bound_variable_name: y.clone(),
            expression: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                bound_variable_name: x.clone(),
                expression: Box::new(LambdaCalculusExpression::Variable { name: y.clone()})
            })
        }),
    };

    assert_eq!(
        z.collect_bound_variable(),
        vec![LambdaCalculusVariableName { string: "a".to_string() }, LambdaCalculusVariableName { string: "b".to_string() }].into_iter().collect(),
    );
}

fn main() {
    println!("Hello, world!");

    {
        let x = LambdaCalculusVariableName { string: "a".to_string() };
        println!("{:?}", x);
        let y = LambdaCalculusExpression::Variable { name: x.clone() };
        println!("{:?}", y);
        let z = LambdaCalculusExpression::Application { function_part: Box::new(y.clone()), argument_part: Box::new(y.clone()) };
        println!("{:?}", z);
        let w = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };
        println!("{:?}", w);
        let v = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(z.clone()) };
        println!("{:?}", v);
    }

    {
        let x = LambdaCalculusVariableName { string: "a".to_string() };
        let y = LambdaCalculusExpression::Variable { name: x.clone() };
        let z = LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

        assert_eq!(x.is_variable_in(&y), true);
        assert_eq!(x.is_variable_in(&z), true);
        assert_eq!(x.is_free_variable_in(&y), true);
        assert_eq!(x.is_free_variable_in(&z), false);
        assert_eq!(x.is_bound_variable_in(&y), false);
        assert_eq!(x.is_bound_variable_in(&z), true);
    }

    {
        let x = LambdaCalculusVariableName { string: "a".to_string() };
        let y = LambdaCalculusVariableName { string: "b".to_string() };
        let z = LambdaCalculusExpression::Application {
            function_part: Box::new(LambdaCalculusExpression::Variable { name: x.clone() }),
            argument_part: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                bound_variable_name: y.clone(),
                expression: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                    bound_variable_name: x.clone(),
                    expression: Box::new(LambdaCalculusExpression::Variable { name: y.clone()})
                })
            }),
        };

        println!("{:?}", z.collect_variable());
        println!("{:?}", z.collect_free_variable());
        println!("{:?}", z.collect_bound_variable());
    }
}
