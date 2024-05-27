pub mod lambda_calculus;

fn main() {
    println!("Hello, world!");

    {
        let x = lambda_calculus::LambdaCalculusVariableName { string: "a".to_string() };
        println!("{:?}", x);
        let y = lambda_calculus::LambdaCalculusExpression::Variable { name: x.clone() };
        println!("{:?}", y);
        let z = lambda_calculus::LambdaCalculusExpression::Application { function_part: Box::new(y.clone()), argument_part: Box::new(y.clone()) };
        println!("{:?}", z);
        let w = lambda_calculus::LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };
        println!("{:?}", w);
        let v = lambda_calculus::LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(z.clone()) };
        println!("{:?}", v);
    }

    {
        let x = lambda_calculus::LambdaCalculusVariableName { string: "a".to_string() };
        let y = lambda_calculus::LambdaCalculusExpression::Variable { name: x.clone() };
        let z = lambda_calculus::LambdaCalculusExpression::LambdaAbstraction { bound_variable_name: x.clone(), expression: Box::new(y.clone()) };

        assert_eq!(x.is_variable_in(&y), true);
        assert_eq!(x.is_variable_in(&z), true);
        assert_eq!(x.is_free_variable_in(&y), true);
        assert_eq!(x.is_free_variable_in(&z), false);
        assert_eq!(x.is_bound_variable_in(&y), false);
        assert_eq!(x.is_bound_variable_in(&z), true);
    }

    {
        let x = lambda_calculus::LambdaCalculusVariableName { string: "a".to_string() };
        let y = lambda_calculus::LambdaCalculusVariableName { string: "b".to_string() };
        let z = lambda_calculus::LambdaCalculusExpression::Application {
            function_part: Box::new(lambda_calculus::LambdaCalculusExpression::Variable { name: x.clone() }),
            argument_part: Box::new(lambda_calculus::LambdaCalculusExpression::LambdaAbstraction {
                bound_variable_name: y.clone(),
                expression: Box::new(lambda_calculus::LambdaCalculusExpression::LambdaAbstraction {
                    bound_variable_name: x.clone(),
                    expression: Box::new(lambda_calculus::LambdaCalculusExpression::Variable { name: y.clone()})
                })
            }),
        };

        println!("{:?}", z.collect_variable());
        println!("{:?}", z.collect_free_variable());
        println!("{:?}", z.collect_bound_variable());
    }
}
