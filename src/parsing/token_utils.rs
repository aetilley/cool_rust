// Utilities for stripping long, deeply nested comments, and for adjusting
// the byte offsets of various code objects that were computed after
// the strip.
// The latter is necessary since we get the spans of our parser objects
// when the parser reads the stripped file, but we want to report the
// location of objects (say to the user during a compilation error)
// with respect to the non-stripped file.

use std::collections::BTreeMap;

use crate::parsing::token::{LexicalError, Token};

use lalrpop_util::ParseError;

#[derive(Debug)]
pub struct CommentError {
    pub msg: String,
}

// Some terminology for the following:
// removal_map:  A map from start bytes of comments (in a text to be
// stripped) to the lenght of that comment.
// insertion_map: A map from bytes (in an already stripped text) immediately
// following the space where a comment was stripped.  That is, if
// i |-> L, then there was a comment of length L in the original text
// between the characters that are now at positions i-1 and i.

pub fn _strip_long_comments_and_get_removal_map(
    input: &str,
) -> Result<(String, BTreeMap<usize, usize>), CommentError> {
    // Remove (* Comments of this form *) which (* May appear anywhere, and
    // which (* May be nested *) arbitrarily deeply *)

    let mut removal_map = BTreeMap::<usize, usize>::new();

    if input.is_empty() {
        return Ok((input.to_string(), removal_map));
    }
    let mut chars = input.chars().enumerate();
    let mut curr = chars.next().unwrap().1;
    let mut look = chars.next();

    let mut comment_depth: i32 = 0;
    let mut result = String::from("");

    let mut comment_start: usize = 0;
    // Offset of the byte being read.
    loop {
        if curr == '(' {
            match look {
                Some((idx, '*')) => {
                    if comment_depth == 0 {
                        // Just started a comment.
                        comment_start = idx - 1;
                    }
                    comment_depth += 1;
                    (curr, look) = match chars.next() {
                        None => break,
                        Some((_, c)) => (c, chars.next()),
                    }
                }
                Some((_, l)) => {
                    if comment_depth == 0 {
                        result.push(curr);
                    }

                    (curr, look) = (l, chars.next());
                }
                None => {
                    if comment_depth == 0 {
                        result.push(curr);
                    };
                    break;
                }
            }
        } else if curr == '*' {
            match look {
                Some((idx, ')')) => {
                    comment_depth -= 1;

                    if comment_depth == 0 {
                        // Just finished a comment.
                        let length = idx + 1 - comment_start;
                        removal_map.insert(comment_start, length);
                    }

                    (curr, look) = match chars.next() {
                        None => break,
                        Some((_, c)) => (c, chars.next()),
                    }
                }
                Some((_, l)) => {
                    if comment_depth == 0 {
                        result.push(curr);
                    }
                    (curr, look) = (l, chars.next());
                }
                None => {
                    if comment_depth == 0 {
                        result.push(curr);
                    };
                    break;
                }
            }
        } else {
            if comment_depth == 0 {
                result.push(curr);
            }
            (curr, look) = match look {
                None => break,
                Some((_, l)) => (l, chars.next()),
            };
        };

        if comment_depth < 0 {
            let msg = "Encountered unmatched *)".to_string();
            return Err(CommentError { msg });
        }
    }
    if comment_depth != 0 {
        let msg = format!(
            "At least one unmatched (*.  (Comment depth was {} upon finishing scan.)",
            comment_depth
        );
        return Err(CommentError { msg });
    }
    Ok((result, removal_map))
}

fn get_insertion_map_from_removal_map(map: &BTreeMap<usize, usize>) -> BTreeMap<usize, usize> {
    let mut result = BTreeMap::<usize, usize>::new();
    let mut removed = 0;
    for (start, length) in map.iter() {
        let new_start = start - removed;
        removed += length;
        result.insert(new_start, *length);
    }
    result
}

pub fn strip_long_comments_and_get_insertion_map(
    input: &str,
) -> Result<(String, BTreeMap<usize, usize>), CommentError> {
    let (text, removal_map) = _strip_long_comments_and_get_removal_map(input)?;
    let insertion_map = get_insertion_map_from_removal_map(&removal_map);
    Ok((text, insertion_map))
}

pub fn get_updated_location(
    original_location: usize,
    insertion_map: &BTreeMap<usize, usize>,
) -> usize {
    // Where a character (not inside a comment) in a stripped file was before stripping.
    let location_increase: usize = insertion_map
        .iter()
        .filter(|(k, _)| k <= &&original_location)
        .map(|(_, v)| v)
        .sum();

    original_location + location_increase
}

pub fn get_updated_span(
    (start, end): (usize, usize),
    insertion_map: &BTreeMap<usize, usize>,
) -> (usize, usize) {
    let new_start = get_updated_location(start, insertion_map);
    let new_end = get_updated_location(end, insertion_map);

    (new_start, new_end)
}

pub fn adjust_locations_in_parse_error(
    err: &mut ParseError<usize, Token, LexicalError>,
    insertion_map: &BTreeMap<usize, usize>,
) {
    // Parsing is done with respect to the comment stripped files, and this the locations in
    // `ParseError`s will not match the original source code.  Here we update them given an
    // insertion_map.
    match err {
        ParseError::InvalidToken { location } => {
            let new_loc = get_updated_location(*location, insertion_map);
            *location = new_loc;
        }
        ParseError::UnrecognizedEof {
            location,
            expected: _,
        } => {
            let new_loc = get_updated_location(*location, insertion_map);
            *location = new_loc;
        }
        ParseError::ExtraToken {
            token: (loc1, _t, loc2),
        } => {
            let new_loc1 = get_updated_location(*loc1, insertion_map);
            let new_loc2 = get_updated_location(*loc2, insertion_map);
            *loc1 = new_loc1;
            *loc2 = new_loc2;
        }
        ParseError::UnrecognizedToken {
            token: (loc1, _t, loc2),
            expected: _,
        } => {
            let new_loc1 = get_updated_location(*loc1, insertion_map);
            let new_loc2 = get_updated_location(*loc2, insertion_map);
            *loc1 = new_loc1;
            *loc2 = new_loc2;
        }
        ParseError::User { error: _ } => {}
    }
}

#[cfg(test)]
mod token_utils_tests {

    use super::*;
    use common_macros::b_tree_map;

    #[test]
    fn strip_comments1() {
        let code: &str = "xyz(*abc(*def*ghi(jkl*)*mno)*)p\nqr";
        let (result_code, removal_map) = _strip_long_comments_and_get_removal_map(code).unwrap();
        let desired_code = "xyzp\nqr";
        assert_eq!(result_code, desired_code);
        let desired_removal_map = b_tree_map! {
            3 => 29 - 3 + 1,
        };
        assert_eq!(removal_map, desired_removal_map);
    }
    #[test]
    fn strip_comments2() {
        let code: &str = "xyz(((*a)bc**))def";
        let (result_code, removal_map) = _strip_long_comments_and_get_removal_map(code).unwrap();
        let desired_code = "xyz(()def";
        assert_eq!(result_code, desired_code);
        let desired_removal_map = b_tree_map! {
            5 => 13 - 5 + 1,
        };
        assert_eq!(removal_map, desired_removal_map);
    }

    #[test]
    fn insertion_map1() {
        let code: &str = "xyz((*a*)defjk((*lm(*no*)pq*)rst(**)uv";
        let (result_code, removal_map) = _strip_long_comments_and_get_removal_map(code).unwrap();
        let desired_code = "xyz(defjk(rstuv";
        assert_eq!(result_code, desired_code);
        let desired_removal_map = b_tree_map! {
            4 => 8 - 4 + 1,
            15 => 28 - 15 + 1,
            32 => 35 - 32 + 1,
        };
        assert_eq!(removal_map, desired_removal_map);
        let insertion_map = get_insertion_map_from_removal_map(&removal_map);
        let desired_insertion_map = b_tree_map! {
            4 => 8 - 4 + 1,
            10 => 28 - 15 + 1,
            13 => 35 - 32 + 1,

        };
        assert_eq!(insertion_map, desired_insertion_map);
        // from z to f
        let span_in_stripped = (2, 6);
        let result = get_updated_span(span_in_stripped, &insertion_map);
        let span_in_unstripped = (2, 11);
        assert_eq!(result, span_in_unstripped);

        // from j to v
        let span_in_stripped = (7, 13);
        let result = get_updated_span(span_in_stripped, &insertion_map);
        let span_in_unstripped = (12, 36);
        assert_eq!(result, span_in_unstripped);
    }
}
