#[derive(PartialEq, Eq, Debug)]
enum Parsing {
    Slice(String),
    Group(Box<Parsing>),
    Repeat(u32, u32, Box<Parsing>),
    Segment(Vec<Parsing>),
}

#[derive(Debug, Clone, Default)]
struct State {
    groups: Vec<String>,
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
        if let Some((n, mut rs)) = parse_plain(&pattern[offset..]) {
            offset += n;
            error_existed = false;
            let mut empty_slice = false;
            if let Some((n, repeat)) = parse_repeat(&pattern[offset..], || {
                let c = match &mut rs {
                    Parsing::Slice(ref mut s) => {
                        let c = s.pop().unwrap();
                        if s.is_empty() { empty_slice = true; }
                        c
                    },
                    _ => unreachable!()
                };
                Parsing::Slice(c.into())
            }) {
                offset += n;
                if empty_slice {
                    segments.push(repeat);
                } else {
                    segments.push(rs);
                    segments.push(repeat);
                }
            } else {
                segments.push(rs);
            }
        } else if error_existed {
            break;
        } else {
            error_existed = true;
        }
        if let Some((n, rs)) = parse_group(&pattern[offset..]) {
            offset += n;
            error_existed = false;
            let mut rs = Some(rs);
            if let Some((n, rs)) = parse_repeat(&pattern[offset..], || rs.take().unwrap()) {
                offset += n;
                segments.push(rs);
            } else {
                segments.push(rs.take().unwrap());
            }
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

fn parse_number(input: &str) -> Option<(usize, u32)> {
    let mut bytes = input.bytes();
    let mut i = if let Some(i) = bytes.next() {
        if !(i >= b'0' && i <= b'9') { return None; }
        (i - b'0') as u32
    } else {
        return None;
    };

    let mut offset = 1;

    while let Some(n) = bytes.next() {
        if !(n >= b'0' && n <= b'9') { break; }
        if i == 0 {
            return None;
        }
        offset += 1;
        i = i * 10 + (n - b'0') as u32;
    }

    Some((offset, i))
}

fn parse_repeat(pattern: &str, ele: impl FnOnce()->Parsing) -> Option<(usize, Parsing)> {
    if pattern.chars().next() != Some('{') {
        return None;
    }
    let (offset, lower_bound) = parse_number(pattern).unwrap_or_default();
    if pattern[1+offset..].chars().next() != Some(',') {
        return None;
    }
    let (offset_add, upper_bound) = parse_number(&pattern[1+offset..]).unwrap_or_else(|| (0, u32::MAX));
    
    if pattern[1+offset+offset_add..].chars().next() != Some('}') {
        return None;
    }

    Some((1+offset+offset_add+1, Parsing::Repeat(lower_bound, upper_bound, Box::new(ele()))))
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
                Ok((slice.bytes().len(), state))
            } else {
                Err(state)
            },
            Parsing::Group(group) => {
                let rs = group.parse_helper(state, tokens); 
                if let Ok((n, mut ok)) = rs {
                    ok.groups.push(tokens[..n].into());
                    Ok((n, ok))
                } else {
                    rs
                }
            }
            Parsing::Segment(segment) => {
                let mut groups = Vec::new();
                let mut offset = 0;
                for s in segment {
                    let (n, state) = s.parse_helper(State::default(), &tokens[offset..])?;
                    offset += n;
                    groups.extend(state.groups);
                }
                Ok((offset, State { groups, ..state }))
            }
            Parsing::Repeat(lower_bound, uppper_bound, parsing) => {
                let mut offset = 0;
                let mut groups = Vec::new();
                let mut count = 0;
                for _ in 0 ..= *uppper_bound {
                    if let Ok((n, state)) = parsing.parse_helper(State::default(), &tokens[offset..]) {
                        offset += n;
                        count += 1;
                        groups.extend(state.groups);
                    } else {
                        break;
                    }
                }
                if count < *lower_bound {
                    Err(state)
                } else {
                    Ok((offset, State { groups, ..state }))
                }
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
