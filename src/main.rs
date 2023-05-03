enum Parsing {
    Slice(String),
    Group(Box<Parsing>),
    Segment(Vec<Parsing>),
}

#[derive(Clone, Default)]
struct State {
    groups: Vec<String>
}

impl State {
    fn new() -> Self {
        Self {
            groups: Vec::new(),
        }
    }
}

impl Parsing {
    fn parse(&self, tokens: &str) -> Option<Vec<String>> {
        if let Ok((n, rs)) = self.parse_helper(State::default(), tokens, 0) {
            if tokens.bytes().len() == n {
                return Some(rs.groups)
            }
        }
        None
    }
    fn parse_helper(&self, mut state: State, tokens: &str, capture_depth: u32) -> Result<(usize, State), State> {
        match self {
            Parsing::Slice(slice) => if tokens.starts_with(slice) {
                for _ in 0 .. capture_depth {
                    state.groups.push(slice.into())
                }
                Ok((slice.bytes().len(), state))
            } else {
                Err(state)
            },
            Parsing::Group(group) => {
                group.parse_helper(state, tokens, capture_depth + 1)
            }
            Parsing::Segment(segment) => {
                let mut segment_groups = Vec::new();
                let mut depth = None;
                let mut offset = 0;
                for s in segment {
                    let (n, state) = s.parse_helper(state.clone(), &tokens[offset..], capture_depth)?;
                    offset += n;
                    if let Some(depth) = depth {
                        if depth != state.groups.len() {
                            panic!()
                        }
                    } else {
                        depth = Some(state.groups.len());
                    }
                    segment_groups.push(state.groups);
                }
                for i in 0 .. segment_groups[0].len() {
                    let mut s = String::new();
                    for j in 0 .. segment_groups.len() {
                        s.push_str(&*segment_groups[j][i])
                    }
                    state.groups.push(s);
                }
                Ok((offset, state))
            }
        }
    }
}

fn main() {
    assert_eq!(Parsing::Slice("abc".into()).parse("abc"), Some(vec![]));
    assert_eq!(Parsing::Group(Box::new(Parsing::Slice("abc".into()))).parse("abc"), Some(vec!["abc".into()]));
}
