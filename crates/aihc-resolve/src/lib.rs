//! Name resolution.
//!
//! The resolver gives every identifier occurrence of a module a target: the
//! top-level entity it names, a local binder, or built-in syntax. The
//! reference is aihc-resolve, run by `tools/resolve-oracle`. Both sides
//! print the same records (see [`record`]), and [`compare`] decides whether
//! a module resolves the same way in both.
//!
//! The resolver itself is not written yet: [`resolve_module`] fails for
//! every module.

pub mod record;

pub use record::{Namespace, Occurrence, Span, Target};

use std::collections::HashMap;

/// The resolver could not resolve a module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResolveError {
    pub message: String,
    pub pos: Option<aihc_syntax::Pos>,
}

/// Every identifier occurrence of a module with its target, sorted by
/// span.
///
/// # Errors
///
/// Fails while the resolver is not implemented.
pub fn resolve_module(_module: &aihc_syntax::ast::Module) -> Result<Vec<Occurrence>, ResolveError> {
    Err(ResolveError {
        message: "resolve: not implemented yet".into(),
        pos: None,
    })
}

/// Where two resolutions of one module differ.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Disagreement {
    pub span: Span,
    pub message: String,
}

/// Whether our occurrences agree with the reference's occurrences of the
/// same module. Both lists must be sorted by span.
///
/// The rules:
///
/// - Both sides see the same spans.
/// - The namespaces agree.
/// - A top-level target agrees when package, module and name are equal.
/// - Local targets agree when they induce the same partition of the
///   spans: two spans share a local id on one side exactly when they do
///   on the other. The ids themselves are not compared.
/// - Syntax agrees with syntax, and an error with an error. Error messages
///   are not compared.
///
/// # Errors
///
/// The first disagreement, in span order.
pub fn compare(ours: &[Occurrence], theirs: &[Occurrence]) -> Result<(), Disagreement> {
    let mut ours_to_theirs: HashMap<u32, u32> = HashMap::new();
    let mut theirs_to_ours: HashMap<u32, u32> = HashMap::new();
    let mut our_next = ours.iter().peekable();
    let mut their_next = theirs.iter().peekable();
    loop {
        let (our, their) = match (our_next.peek(), their_next.peek()) {
            (None, None) => return Ok(()),
            (Some(our), None) => {
                return Err(missing(our.span, "the reference has no occurrence here"))
            }
            (None, Some(their)) => return Err(missing(their.span, "we have no occurrence here")),
            (Some(our), Some(their)) => (*our, *their),
        };
        if our.span < their.span {
            return Err(missing(our.span, "the reference has no occurrence here"));
        }
        if their.span < our.span {
            return Err(missing(their.span, "we have no occurrence here"));
        }
        our_next.next();
        their_next.next();
        if our.namespace != their.namespace {
            return Err(Disagreement {
                span: our.span,
                message: format!(
                    "namespace: ours {}, reference {}",
                    our.namespace, their.namespace
                ),
            });
        }
        match (&our.target, &their.target) {
            (Target::Top { .. }, Target::Top { .. }) if our.target == their.target => {}
            (Target::Local(our_id), Target::Local(their_id)) => {
                let forward = *ours_to_theirs.entry(*our_id).or_insert(*their_id);
                let backward = *theirs_to_ours.entry(*their_id).or_insert(*our_id);
                if forward != *their_id || backward != *our_id {
                    return Err(Disagreement {
                        span: our.span,
                        message: "local binder: the spans of this binder differ from the reference"
                            .into(),
                    });
                }
            }
            (Target::Syntax, Target::Syntax) | (Target::Error(_), Target::Error(_)) => {}
            _ => {
                return Err(Disagreement {
                    span: our.span,
                    message: format!("target: ours {}, reference {}", our.target, their.target),
                })
            }
        }
    }
}

fn missing(span: Span, message: &str) -> Disagreement {
    Disagreement {
        span,
        message: message.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn occ(line: u32, target: Target) -> Occurrence {
        Occurrence {
            span: Span {
                start_line: line,
                start_col: 1,
                end_line: line,
                end_col: 2,
            },
            namespace: Namespace::Term,
            target,
        }
    }

    fn top(name: &str) -> Target {
        Target::Top {
            package: "base".into(),
            module: "GHC.Base".into(),
            name: name.into(),
        }
    }

    #[test]
    fn equal_top_level_targets_agree() {
        let ours = [occ(1, top("map")), occ(2, Target::Syntax)];
        assert_eq!(compare(&ours, &ours), Ok(()));
    }

    #[test]
    fn different_top_level_targets_disagree() {
        let ours = [occ(1, top("map"))];
        let theirs = [occ(1, top("fmap"))];
        let err = compare(&ours, &theirs).unwrap_err();
        assert_eq!(err.span.start_line, 1);
        assert!(err.message.starts_with("target:"), "{}", err.message);
    }

    #[test]
    fn local_ids_compare_as_a_partition() {
        // Different numbers, same partition: {1, 3} and {2}.
        let ours = [
            occ(1, Target::Local(7)),
            occ(2, Target::Local(8)),
            occ(3, Target::Local(7)),
        ];
        let theirs = [
            occ(1, Target::Local(0)),
            occ(2, Target::Local(1)),
            occ(3, Target::Local(0)),
        ];
        assert_eq!(compare(&ours, &theirs), Ok(()));
        // Same numbers, different partition.
        let merged = [
            occ(1, Target::Local(0)),
            occ(2, Target::Local(0)),
            occ(3, Target::Local(0)),
        ];
        assert_eq!(compare(&theirs, &merged).unwrap_err().span.start_line, 2);
        assert_eq!(compare(&merged, &theirs).unwrap_err().span.start_line, 2);
    }

    #[test]
    fn a_missing_span_disagrees() {
        let ours = [occ(1, top("map"))];
        let theirs = [occ(1, top("map")), occ(2, top("id"))];
        assert_eq!(compare(&ours, &theirs).unwrap_err().span.start_line, 2);
        assert_eq!(compare(&theirs, &ours).unwrap_err().span.start_line, 2);
    }

    #[test]
    fn error_messages_are_not_compared() {
        let ours = [occ(1, Target::Error("unbound".into()))];
        let theirs = [occ(1, Target::Error("not found".into()))];
        assert_eq!(compare(&ours, &theirs), Ok(()));
    }
}
