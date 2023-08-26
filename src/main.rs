use std::collections::HashSet;

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
                let x_left = term_left.is_used(name_target);
                let x_right = term_right.is_used(name_target);
                x_left || x_right
            }
            TermLabdaPure::Lam { name, term } => {
                let x_var = name == name_target;
                let x_term = term.is_used(name_target);
                x_var || x_term
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                let x_copied = name_copied == name_target;
                let x_copy_1 = name_copy_1 == name_target;
                let x_copy_2 = name_copy_2 == name_target;
                let x_term = term.is_used(name_target);
                x_copied || x_copy_1 || x_copy_2 || x_term
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x_dropped = name_dropped == name_target;
                let x_term = term.is_used(name_target);
                x_dropped || x_term
            }
        }
    }

    fn is_used_bound(&self, name_target: &String) -> bool {
        match self {
            TermLabdaPure::Var { .. } => false,
            TermLabdaPure::App { term_left, term_right, .. } => {
                let x = term_left.is_used_bound(name_target);
                let y = term_right.is_used_bound(name_target);
                x || y
            }
            TermLabdaPure::Lam { name, term } => {
                let x_var = name == name_target;
                let x_term = term.is_used_bound(name_target);
                x_var || x_term
            }
            TermLabdaPure::Dup { name_copy_1, name_copy_2, term, .. } => {
                let x_copy_1 = name_copy_1 == name_target;
                let x_copy_2 = name_copy_2 == name_target;
                let x_term = term.is_used_bound(name_target);
                x_copy_1 || x_copy_2 || x_term
            }
            TermLabdaPure::Des { term, .. } => {
                term.is_used_bound(name_target)
            }
        }
    }

    fn is_used_free(&self, name_target: &String) -> bool {
        match self {
            TermLabdaPure::Var { name } => name == name_target,
            TermLabdaPure::App { term_left, term_right, .. } => {
                let x_left = term_left.is_used_free(name_target);
                let x_right = term_right.is_used_free(name_target);
                x_left || x_right
            }
            TermLabdaPure::Lam { name, term } => {
                let x_var = name == name_target;
                let x_term = term.is_used_free(name_target);
                !x_var && x_term
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                let x_copied = name_copied == name_target;
                let x_copy_1 = name_copy_1 == name_target;
                let x_copy_2 = name_copy_2 == name_target;
                let x_term = term.is_used_free(name_target);
                let x_lambda = !x_copy_1 && !x_copy_2 && x_term;
                x_copied || x_lambda
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x_dropped = name_dropped == name_target;
                let x_term = term.is_used_free(name_target);
                x_dropped || x_term
            }
        }
    }

    fn is_used_conf(
        &self,
        flag_bound: bool,
        flag_free: bool,
        name_target: &String,
    ) -> bool {
        match self {
            TermLabdaPure::Var { name } => {
                if flag_free {
                    name == name_target
                } else {
                    false
                }
            }
            TermLabdaPure::App { term_left, term_right, .. } => {
                let x_left = term_left.is_used_conf(
                    flag_bound,
                    flag_free,
                    name_target,
                );
                let x_right = term_right.is_used_conf(
                    flag_bound,
                    flag_free,
                    name_target,
                );
                x_left || x_right
            }
            TermLabdaPure::Lam { name, term } => {
                let x_var = name == name_target;
                let x_term = term.is_used_conf(
                    flag_bound,
                    flag_free,
                    name_target,
                );
                if x_var {
                    flag_bound
                } else {
                    x_term
                }
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                let x_copied = if flag_free {
                    name_copied == name_target
                } else {
                    false
                };
                let x_copy_1 = name_copy_1 == name_target;
                let x_copy_2 = name_copy_2 == name_target;
                let x_term = term.is_used_conf(
                    flag_bound,
                    flag_free,
                    name_target,
                );
                let x_lambda = if x_copy_1 || x_copy_2 {
                    flag_bound
                } else {
                    x_term
                };
                x_copied || x_lambda
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x_dropped = if flag_free {
                    name_dropped == name_target
                } else {
                    false
                };
                let x_term = term.is_used_conf(
                    flag_bound,
                    flag_free,
                    name_target,
                );
                x_dropped || x_term
            }
        }
    }

    fn set_var(&self) -> HashSet<String> {
        match self {
            TermLabdaPure::Var { name } => HashSet::new().insert(name),
            TermLabdaPure::App { term_left, term_right, .. } => {
                let x_left = term_left.set_var();
                let x_right = term_right.set_var();
                x_left.extend(&x_right)
            }
            TermLabdaPure::Lam { name, term } => {
                let x_var = HashSet::new().insert(name);
                let x_term = term.set_var();
                x_term.extend(&x_var)
            }
            TermLabdaPure::Dup {
                name_copied,
                name_copy_1,
                name_copy_2,
                term,
            } => {
                let x_copied = HashSet::new().insert(name_copied);
                let x_copy_1 = HashSet::new().insert(name_copy_1);
                let x_copy_2 = HashSet::new().insert(name_copy_2);
                let x_term = term.set_var();
                x_term.extend(&x_copied).extend(&x_copy_1).extend(&x_copy_2)
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x_dropped = HashSet::new().insert(name_dropped);
                let x_term = term.set_var();
                x_term.extend(&x_dropped)
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
