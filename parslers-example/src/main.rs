mod arrays {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/arrays.rs"));
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_one_normal() {
        let input = "[a, a, a]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert_eq!(result, Ok(vec!['a', 'a', 'a']));
    }

    #[test]
    fn array_one_empty() {
        let input = "[]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert_eq!(result, Ok(vec![]));
    }

    #[test]
    fn array_one_missing_element() {
        let input = "[a, , a]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), ", a]");
    }

    #[test]
    fn array_one_missing_comma() {
        let input = "[a a a]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        println!("{:?}", result);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "a a]");
    }

    #[test]
    fn array_one_missing_opening_bracket() {
        let input = "a, a, a]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "a, a, a]");
    }

    #[test]
    fn array_one_missing_closing_bracket() {
        let input = "[a, a, a";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "");
    }

    #[test]
    fn array_one_wrong_delimiter() {
        let input = "[a, a; a]";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "; a]");
    }

    #[test]
    fn array_two_normal() {
        let input = "{b; b; b}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert_eq!(result, Ok(vec!['b', 'b', 'b']));
    }

    #[test]
    fn array_two_empty() {
        let input = "{}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert_eq!(result, Ok(vec![]));
    }

    #[test]
    fn array_two_missing_element() {
        let input = "{b; ; b}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "; b}");
    }

    #[test]
    fn array_two_missing_comma() {
        let input = "{b b b}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        println!("{:?}", result);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "b b}");
    }

    #[test]
    fn array_two_missing_opening_bracket() {
        let input = "b; b; b}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "b; b; b}");
    }

    #[test]
    fn array_two_missing_closing_bracket() {
        let input = "{b; b; b";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), "");
    }

    #[test]
    fn array_two_wrong_delimiter() {
        let input = "{b; b, b}";
        let chars = &mut input.chars();

        let result = arrays::arrays(chars);
        assert!(result.is_err());
        assert_eq!(chars.as_str(), ", b}");
    }
}
