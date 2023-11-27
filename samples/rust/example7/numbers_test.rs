mod numbers;

#[cfg(test)]
mod test {
     use super::numbers::square;

     #[test]
     fn test_non_negative_squares() {
         for i in 0..100 {
            assert_eq!(square(i), i * i);
         }
     }

     #[test]
     fn test_negative_squares() {
         for i in -100..0 {
            assert_eq!(square(i), i * i);
         }
     }
}