use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct LambdaCalculusVariableName { string: String }

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum LambdaCalculusExpression {
    Variable { name: LambdaCalculusVariableName },
    Application { function_part: Box<LambdaCalculusExpression>, argument_part: Box<LambdaCalculusExpression> },
    LambdaAbstraction { variable_name: LambdaCalculusVariableName, expression: Box<LambdaCalculusExpression> },
}

impl LambdaCalculusVariableName {
    fn is_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name } =>
                self == name,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_variable_in(function_part) || self.is_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } =>
                self == variable_name || self.is_variable_in(expression),
        }
    }

    fn is_free_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name } =>
                self == name,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_free_variable_in(function_part) || self.is_free_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } =>
                self != variable_name && self.is_free_variable_in(expression),
        }
    }

    fn is_bound_variable_in(&self, expression: &LambdaCalculusExpression) -> bool {
        match expression {
            LambdaCalculusExpression::Variable { name: _ } =>
                false,

            LambdaCalculusExpression::Application { function_part, argument_part } =>
                self.is_bound_variable_in(function_part) || self.is_bound_variable_in(argument_part),

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } =>
                self == variable_name || self.is_bound_variable_in(expression),
        }
    }
}

impl LambdaCalculusExpression {
    fn update_variable_set(expression: &LambdaCalculusExpression, set: &mut HashSet<LambdaCalculusVariableName>) {
        match expression {
            LambdaCalculusExpression::Variable { name } => {
                set.insert(name.clone());
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                Self::update_variable_set(function_part, set);
                Self::update_variable_set(argument_part, set);
            }

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } => {
                Self::update_variable_set(expression, set);
                set.insert(variable_name.clone());
            }
        }
    }

    fn collect_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        let mut set = HashSet::new();

        Self::update_variable_set(self, &mut set);

        set
    }

    fn update_free_variable_set(expression: &LambdaCalculusExpression, set: &mut HashSet<LambdaCalculusVariableName>) {
        match expression {
            LambdaCalculusExpression::Variable { name } => {
                set.insert(name.clone());
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                Self::update_free_variable_set(function_part, set);
                Self::update_free_variable_set(argument_part, set);
            }

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } => {
                Self::update_free_variable_set(expression, set);
                set.remove(variable_name);
            }
        }
    }

    fn collect_free_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        let mut set = HashSet::new();

        Self::update_free_variable_set(self, &mut set);

        set
    }

    fn update_bound_variable_set(expression: &LambdaCalculusExpression, set: &mut HashSet<LambdaCalculusVariableName>) {
        match expression {
            LambdaCalculusExpression::Variable { name: _ } => {
            }

            LambdaCalculusExpression::Application { function_part, argument_part } => {
                Self::update_bound_variable_set(function_part, set);
                Self::update_bound_variable_set(argument_part, set);
            }

            LambdaCalculusExpression::LambdaAbstraction { variable_name, expression } => {
                Self::update_bound_variable_set(expression, set);
                set.insert(variable_name.clone());
            }
        }
    }

    fn collect_bound_variable(&self) -> HashSet<LambdaCalculusVariableName> {
        let mut set = HashSet::new();

        Self::update_bound_variable_set(self, &mut set);

        set
    }
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
        let w = LambdaCalculusExpression::LambdaAbstraction { variable_name: x.clone(), expression: Box::new(y.clone()) };
        println!("{:?}", w);
        let v = LambdaCalculusExpression::LambdaAbstraction { variable_name: x.clone(), expression: Box::new(z.clone()) };
        println!("{:?}", v);
    }

    {
        let x = LambdaCalculusVariableName { string: "a".to_string() };
        let y = LambdaCalculusExpression::Variable { name: x.clone() };
        let z = LambdaCalculusExpression::LambdaAbstraction { variable_name: x.clone(), expression: Box::new(y.clone()) };

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
                variable_name: y.clone(),
                expression: Box::new(LambdaCalculusExpression::LambdaAbstraction {
                    variable_name: x.clone(),
                    expression: Box::new(LambdaCalculusExpression::Variable { name: y.clone()})
                })
            }),
        };

        println!("{:?}", z.collect_variable());
        println!("{:?}", z.collect_free_variable());
        println!("{:?}", z.collect_bound_variable());
    }
}
