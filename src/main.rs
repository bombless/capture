#[derive(PartialEq, Eq, Debug)]
enum Parsing {
    Slice(String),
    Group(Box<Parsing>),
    Segment(Vec<Parsing>),
}

#[derive(Debug, Clone, Default)]
struct State {
    groups: Vec<String>,
    history: String,
}

/*

plain = [^()]+
content = (plain | group)+
group = '(' content ')'
 */

fn parse_pattern(pattern: &str) -> Option<Parsing> {
    if let Some((n, rs)) = parse_content(pattern) {
        if n == pattern.bytes().len() {
            return Some(rs)
        }
    }
    None
}

fn parse_content(pattern: &str) -> Option<(usize, Parsing)> {
    let mut offset = 0;
    let mut error_existed = false;
    let mut segments = Vec::new();
    loop {
        if let Some((n, rs)) = parse_plain(&pattern[offset..]) {
            offset += n;
            error_existed = false;
            segments.push(rs);
        } else if error_existed {
            break;
        } else {
            error_existed = true;
        }
        if let Some((n, rs)) = parse_group(&pattern[offset..]) {
            offset += n;
            error_existed = false;
            segments.push(rs);
        } else if error_existed {
            break;
        } else {
            error_existed = true;
        }
    }
    if segments.is_empty() {
        None
    } else if segments.len() == 1 {
        Some((offset, segments.pop().unwrap()))
    } else {
        Some((offset, Parsing::Segment(segments)))
    }
}

fn parse_plain(pattern: &str) -> Option<(usize, Parsing)> {
    let mut plain = String::new();
    for (offset, c) in pattern.char_indices() {
        if c == '(' || c == ')' {
            if offset > 0 {
                return Some((offset, Parsing::Slice(plain.into())));
            } else {
                return None;
            }
        }
        plain.push(c);
    }
    if plain.is_empty() {
        return None;
    } else {
        return Some((plain.bytes().len(), Parsing::Slice(plain.into())));
    }
}

fn parse_group(pattern: &str) -> Option<(usize, Parsing)> {
    if pattern.chars().next() != Some('(') {
        return None;
    }
    if let Some((n, rs)) = parse_content(&pattern[1..]) {
        if pattern.bytes().len() > 1 + n && pattern.as_bytes()[1+n] == b')' {
            return Some((2 + n, Parsing::Group(Box::new(rs))));
        }
    }
    None
}

impl Parsing {
    fn new(pattern: &str) -> Option<Self> {
        parse_pattern(pattern)
    }
    fn parse(&self, tokens: &str) -> Option<Vec<String>> {
        if let Ok((n, rs)) = self.parse_helper(State::default(), tokens) {
            if tokens.bytes().len() == n {
                return Some(rs.groups)
            }
        }
        None
    }
    fn parse_helper(&self, state: State, tokens: &str) -> Result<(usize, State), State> {
        match self {
            Parsing::Slice(slice) => if tokens.starts_with(slice) {
                Ok((slice.bytes().len(), State { history: slice.into(), ..state }))
            } else {
                Err(state)
            },
            Parsing::Group(group) => {
                let rs = group.parse_helper(state, tokens); 
                if let Ok((n, mut ok)) = rs {
                    ok.groups.push(ok.history.clone());
                    Ok((n, ok))
                } else {
                    rs
                }
            }
            Parsing::Segment(segment) => {
                let mut groups = Vec::new();
                let mut history = String::new();
                let mut offset = 0;
                for s in segment {
                    let (n, state) = s.parse_helper(state.clone(), &tokens[offset..])?;
                    offset += n;
                    history.push_str(&state.history);
                    groups.extend(state.groups);
                }
                Ok((offset, State { history, groups }))
            }
        }
    }
}

fn main() {
    assert_eq!(Parsing::Slice("abc".into()).parse("abc"), Some(vec![]));
    assert_eq!(Parsing::Group(Box::new(Parsing::Slice("abc".into()))).parse("abc"), Some(vec!["abc".into()]));
    assert_eq!(Parsing::new("(abc)").unwrap(), Parsing::Group(Box::new(Parsing::Slice("abc".into()))));
    assert_eq!(Parsing::new("abc").unwrap().parse("abc"), Some(vec![]));
    let parser = Parsing::new("(abc)").unwrap();
    let rs = parser.parse("abc").unwrap();
    assert_eq!(rs, ["abc"]);

    assert_eq!(Parsing::new("(a)b(c)").unwrap().parse("abc").unwrap(), ["a", "c"]);
}
