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
pub struct LambdaAbstractionExpression { bound_variable_name: VariableName, expression: Box<Expression> }

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

                set.extend(expression.collect_variable());
                set.insert(bound_variable_name.clone());

                set
            }
        }
    }

    pub fn beta_reduce(function_part: &LambdaAbstractionExpression, argument_part: &Expression) -> Expression {
        match function_part {
            LambdaAbstractionExpression { bound_variable_name: _, expression: _ } => {
                todo!();
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
