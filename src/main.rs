use std::collections::HashSet;

static LITERAL_SPECIAL: &'static [char] = &['(', ')', '{', '}', '+', '*', '?', '[', ']'];

#[derive(PartialEq, Eq, Debug)]
enum Parsing {
    Slice(String),
    Group(Box<Parsing>),
    Repeat(u32, u32, Box<Parsing>),
    Segment(Vec<Parsing>),
    Set(Set),
}

#[derive(Eq, PartialEq, Debug)]
struct Set(IncludeMode, HashSet<char>);

impl Set {
    fn includes(&self, c: char) -> bool {
        match &self.0 {
            IncludeMode::Include => self.1.contains(&c),
            IncludeMode::Exclude => !self.1.contains(&c),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum IncludeMode {
    Include,
    Exclude,
}

#[derive(Debug, Clone, Default)]
struct State {
    groups: Vec<String>,
}

/*

plain = [^()]+
content = (plain | group | set)+
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
    let mut error_plain = false;
    let mut error_group = false;
    let mut error_set = false;
    let mut segments = Vec::new();
    loop {
        if let Some((n, mut rs)) = parse_plain(&pattern[offset..]) {
            offset += n;
            error_plain = false;
            error_group = false;
            error_set = false;
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
        } else if error_plain {
            break;
        } else {
            error_plain = true;
        }
        fn handle_repeat(pattern: &str, offset: &mut usize, rs: Parsing) -> Parsing {
            let mut rs = Some(rs);
            if let Some((n, rs)) = parse_repeat(&pattern[*offset..], || rs.take().unwrap()) {
                *offset += n;
                rs
            } else {
                rs.take().unwrap()
            }
        }
        if let Some((n, rs)) = parse_group(&pattern[offset..]) {
            offset += n;
            error_plain = false;
            error_group = false;
            error_set = false;
            let rs = handle_repeat(pattern, &mut offset, rs);
            segments.push(rs);
        } else if error_group {
            break;
        } else {
            error_group = true;
        }
        if let Some((n, rs)) = parse_set(&pattern[offset..]) {
            offset += n;
            error_plain = false;
            error_group = false;
            error_set = false;
            let rs = handle_repeat(pattern, &mut offset, rs);
            segments.push(rs);
        } else if error_set {
            break;
        } else {
            error_set = true;
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
    let mut escaping = false;
    let mut offset = 0;
    for (this_offset, c) in pattern.char_indices() {
        offset = this_offset;
        if c == '\\' {
            if escaping {
                plain.push(c);
                escaping = false;
            } else {
                escaping = true;
            }
            continue;
        }
        if LITERAL_SPECIAL.iter().any(|&x| x == c) {
            if escaping {
                plain.push(c);
                escaping = false;
                continue;
            }
            else if offset > 0 {
                return Some((offset, Parsing::Slice(plain)));
            } else {
                return None;
            }
        }
        plain.push(c);
    }
    if plain.is_empty() || escaping {
        return None;
    } else {
        return Some((offset + 1, Parsing::Slice(plain)));
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

fn parse_set(pattern: &str) -> Option<(usize, Parsing)> {
    let mut iter = pattern.char_indices();
    match iter.next() {
        Some((_, '[')) => {},
        _ => return None,
    }
    
    let mut set = HashSet::new();
    let first = iter.next()?.1;
    let mode = if first == '^' {
        IncludeMode::Exclude
    } else {
        set.insert(first);
        IncludeMode::Include
    };
    loop {
        let (offset, c) = iter.next()?;
        if c == ']' {
            return Some((offset + 1, Parsing::Set(Set(mode, set))));
        }
        if c == '\\' {
            let c = iter.next()?.1;
            if ['[', ']'].iter().any(|&x| x == c) {
                set.insert(c);
            } else {
                return None;
            }
        } else {
            set.insert(c);
        }
    }
}

fn parse_number(input: &str) -> Option<(usize, u32)> {
    let mut bytes = input.bytes();
    let first = bytes.next();
    let mut i = if let Some(i) = first {
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
    match pattern.chars().next() {
        Some('{') => {},
        Some('+') => return Some((1, Parsing::Repeat(1, u32::MAX, Box::new(ele())))),
        Some('*') => return Some((1, Parsing::Repeat(0, u32::MAX, Box::new(ele())))),
        Some('?') => return Some((1, Parsing::Repeat(0, 1, Box::new(ele())))),
        _ => return None,
    }
    if pattern.chars().next() != Some('{') {
        return None;
    }
    let (offset, lower_bound) = parse_number(&pattern[1..]).unwrap_or_default();
    if pattern[1+offset..].chars().next() != Some(',') {
        return None;
    }
    let (offset_add, upper_bound) = parse_number(&pattern[1+offset+1..]).unwrap_or_else(|| (0, u32::MAX));
    
    if pattern[1+offset+1+offset_add..].chars().next() != Some('}') {
        return None;
    }

    Some((1+offset+1+offset_add+1, Parsing::Repeat(lower_bound, upper_bound, Box::new(ele()))))
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
            Parsing::Set(set) => if tokens.chars().next().map_or(false, |x| set.includes(x)) {
                Ok((1, state))
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
                for _ in 0 .. *uppper_bound {
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

fn group(p: Parsing) -> Parsing {
    Parsing::Group(Box::new(p))
}
fn repeat(lower_bound: u32, upper_bound: u32, p: Parsing) -> Parsing {
    Parsing::Repeat(lower_bound, upper_bound, Box::new(p))
}
fn slice(s: &str) -> Parsing {
    Parsing::Slice(s.into())
}
fn segment<const N: usize>(s: [Parsing; N]) -> Parsing {
    Parsing::Segment(s.into())
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

    assert_eq!(Parsing::new("a{1,}").unwrap(), repeat(1, u32::MAX, slice("a")));

    let maybe_parsing = Parsing::new("(a{,})");
    let parsing = maybe_parsing.unwrap();
    assert_eq!(parsing, group(repeat(0, u32::MAX, slice("a"))));
    assert_eq!(parsing.parse("a").unwrap(), ["a"]);

    let maybe_parsing = Parsing::new("(a{,})b");
    let parsing = maybe_parsing.unwrap();
    assert_eq!(parsing, segment([group(repeat(0, u32::MAX, slice("a"))), slice("b")]));
    assert_eq!(parsing.parse("ab").unwrap(), ["a"]);

    let maybe_parsing = Parsing::new("(a?b)c");
    let parsing = maybe_parsing.unwrap();
    assert_eq!(parsing, segment([group(segment([repeat(0, 1, slice("a")), slice("b")])), slice("c")]));
    assert_eq!(parsing.parse("bc").unwrap(), ["b"]);
    assert_eq!(parsing.parse("aabc"), None);

    Parsing::new("[0][1]").unwrap();

    let parse_number = Parsing::new("([123456789][0123456789]*)").unwrap();
    assert_eq!(parse_number.parse("01"), None);
    assert_eq!(parse_number.parse("42"), Some(vec!["42".into()]));

    assert_eq!(Parsing::new("(\\(\\\\)").unwrap().parse("(\\").unwrap(), ["(\\"]);

    let greedy_parser = Parsing::new("(a+)a").unwrap();

    let rs = greedy_parser.parse("aa");

    assert_eq!(rs, Some(vec!["a".into()]));

    assert_eq!(Parsing::new("(\\([\\[\\]]+)\\]").unwrap().parse("(]]]").unwrap(), ["(]]"]);


}
