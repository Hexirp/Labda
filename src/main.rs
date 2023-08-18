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
                let x = term_left.is_used(name_target);
                let y = term_right.is_used(name_target);
                x || y
            }
            TermLabdaPure::Lam { name, term } => {
                let x = name == name_target;
                if x {
                    true
                } else {
                    term.is_used(name_target)
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
                    true
                } else {
                    term.is_used(name_target)
                };
                x || w
            }
            TermLabdaPure::Des { name_dropped, term } => {
                let x = name_dropped == name_target;
                let y = term.is_used(name_target);
                x || y
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
                let x = name == name_target;
                if x {
                    true
                } else {
                    term.is_used_bound(name_target)
                }
            }
            TermLabdaPure::Dup { name_copy_1, name_copy_2, term, .. } => {
                let x = name_copy_1 == name_target;
                let y = name_copy_2 == name_target;
                if x || y {
                    true
                } else {
                    term.is_used_bound(name_target)
                }
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
                let x = name == name_target;
                if x {
                    flag_bound
                } else {
                    term.is_used_conf(flag_bound, flag_free, name_target)
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
                let x_lambda = if x_copy_1 || x_copy_2 {
                    flag_bound
                } else {
                    term.is_used_conf(flag_bound, flag_free, name_target)
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
}

fn main() {
    println!("Hello, world!");
}
