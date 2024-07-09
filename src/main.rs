pub mod lambda_calculus;

fn reduce_once(text: String) -> String {
    return text;
}

#[test]
fn test_reduce_once_x() {
    let x = "x".to_string();

    assert_eq!(reduce_once(x), "".to_string());
}

fn main() {
    println!("Hello, world!");
}
