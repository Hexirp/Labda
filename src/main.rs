#[derive(Clone, PartialEq, Eq, Debug)]
struct LambdaCalculusVariableName { string: String }

#[derive(Clone, PartialEq, Eq, Debug)]
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
}

fn main() {
    println!("Hello, world!");
    let x_000 = LambdaCalculusVariableName { string: "a".to_string() };
    println!("{:?}", x_000);
    let x_001 = LambdaCalculusExpression::Variable { name: x_000.clone() };
    println!("{:?}", x_001);
    let x_002 = LambdaCalculusExpression::Application { function_part: Box::new(x_001.clone()), argument_part: Box::new(x_001.clone()) };
    println!("{:?}", x_002);
    let x_003 = LambdaCalculusExpression::LambdaAbstraction { variable_name: x_000.clone(), expression: Box::new(x_001.clone()) };
    println!("{:?}", x_003);
    let x_004 = LambdaCalculusExpression::LambdaAbstraction { variable_name: x_000.clone(), expression: Box::new(x_002.clone()) };
    println!("{:?}", x_004);
    let x_005 = &x_000.is_variable_in(&x_001);
    println!("{:?}", x_005);
}
