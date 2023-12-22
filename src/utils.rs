pub fn comma_sep(s: &str) -> String {
    let mut ret = String::with_capacity(s.len() + (s.len() / 3));
    let (before_comma, mut after_comma) = s.split_at(s.len() % 3);
    ret.push_str(before_comma);
    while !after_comma.is_empty() {
        if !ret.is_empty() {
            ret.push(',');
        }
        let (before_comma, after_comma_) = after_comma.split_at(3);
        ret.push_str(before_comma);
        after_comma = after_comma_;
    }
    ret
}

#[test]
fn test_comma_sep() {
    assert_eq!("", comma_sep(""));
    assert_eq!("0", comma_sep("0"));
    assert_eq!("00", comma_sep("00"));
    assert_eq!("000", comma_sep("000"));
    assert_eq!("0,000", comma_sep("0000"));
    assert_eq!("000,000", comma_sep("000000"));
    assert_eq!("0,000,000", comma_sep("0000000"));
}
