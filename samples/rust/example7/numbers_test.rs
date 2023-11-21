mod numbers;

#[cfg(test)]
mod test {
     use super::numbers::square;

     #[test]
     fn test_non_negative_squares() {
         assert_eq!(square(5), 25);
     }

     #[test]
     fn test_negative_squares() {
         assert_eq!(square(-3), 9);
     }

     #[test]
     fn test_fail() {
         assert_eq!(square(1), 0);
     }
}