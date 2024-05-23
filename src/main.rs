#[derive(Clone, Debug)]
struct LambdaCalculusVariableName { string: String }

#[derive(Clone, Debug)]
enum LambdaCalculusExpression {
    Variable { name: LambdaCalculusVariableName },
    Application { function_part: Box<LambdaCalculusExpression>, argument_part: Box<LambdaCalculusExpression> },
    LambdaAbstraction { variable_name: LambdaCalculusVariableName, expression: Box<LambdaCalculusExpression> },
}

fn main() {
    println!("Hello, world!");
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
