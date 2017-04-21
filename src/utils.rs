pub fn is_letter(s: &String) -> bool {
    &"a".to_string() <= s && s <= &"z".to_string() ||
    &"A".to_string() <= s && s <= &"Z".to_string() || &"_".to_string() == s
}

pub fn is_digit(s: &String) -> bool {
    let c = s.chars().nth(0);
    match c {
        Some(n) => n.is_digit(10),
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_detect_character() {
        assert!(is_letter(&"a".to_string()));
        assert!(is_letter(&"Z".to_string()));
        assert!(is_letter(&"_".to_string()));
        assert!(!is_letter(&"-".to_string()));
        assert!(!is_letter(&"あ".to_string()));
        assert!(!is_letter(&" ".to_string()));
        assert!(!is_letter(&"漢".to_string()));
    }

    #[test]
    fn it_should_detect_digit() {
        assert!(!is_digit(&"a".to_string()));
        assert!(!is_digit(&"Z".to_string()));
        assert!(!is_digit(&"_".to_string()));
        assert!(is_digit(&"0".to_string()));
        assert!(is_digit(&"9".to_string()));
    }
}
