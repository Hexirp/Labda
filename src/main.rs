enum TermLabdaPure {
    Var { name: String },
    App {
        term_left: Box<TermLabdaPure>,
        term_right: Box<TermLabdaPure>,
        strategy: bool,
    },
    Lam { name: String, term: Box<TermLabdaPure> },
    Dup {
        name_copied: String,
        name_copy_1: String,
        name_copy_2: String,
        term: Box<TermLabdaPure>,
    },
    Des { name_dropped: String, term: Box<TermLabdaPure> },
}

impl TermLabdaPure {
    fn is_used(&self, name_target: &String) -> bool {
        match self {
            TermLabdaPure::Var { name } => name == name_target,
            TermLabdaPure::App { term_left, term_right, .. } => {
                false
                    || term_left.is_used(name_target)
                    || term_right.is_used(name_target)
            }
            TermLabdaPure::Lam { name, term } => {
                false
                    || name == name_target
                    || term.is_used(name_target)
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                false
                    || name_copied == name_target
                    || name_copy_1 == name_target
                    || name_copy_2 == name_target
                    || term.is_used(name_target)
            }
            TermLabdaPure::Des { name_dropped, term } => {
                name_dropped == name_target || term.is_used(name_target)
            }
        }
    }

    fn is_used_bound(&self, name_target: &String) -> bool {
        match self {
            TermLabdaPure::Var { name } => false,
            TermLabdaPure::App { term_left, term_right, .. } => {
                false
                    || term_left.is_used_bound(name_target)
                    || term_right.is_used_bound(name_target)
            }
            TermLabdaPure::Lam { name, term } => {
                false
                    || name == name_target
                    || term.is_used_bound(name_target)
            }
            TermLabdaPure::Dup { name_copy_1, name_copy_2, term, .. } => {
                false
                    || name_copy_1 == name_target
                    || name_copy_2 == name_target
                    || term.is_used_bound(name_target)
            }
            TermLabdaPure::Des { name_dropped, term } => {
                term.is_used_bound(name_target)
            }
        }
    }

    fn is_used_free(&self, name_target: &String) -> bool {
        match self {
            TermLabdaPure::Var { name } => name == name_target,
            TermLabdaPure::App { term_left, term_right, .. } => {
                let x = term_left.is_used_free(name_target);
                let y = term_right.is_used_free(name_target);
                x || y
            }
            TermLabdaPure::Lam { name, term } => {
                if name == name_target {
                    false
                } else {
                    term.is_used_free(name_target)
                }
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                let x = name_copied == name_target;
                let y = name_copy_1 == name_target;
                let z = name_copy_2 == name_target;
                let w = if y || z {
                    false
                } else {
                    term.is_used_free(name_target)
                };
                x || w
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x = name_dropped == name_target;
                let y = term.is_used_free(name_target);
                x || y
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
