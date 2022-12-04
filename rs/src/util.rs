pub struct ArrayChunks<'a, const N: usize, T> {
    remaining: &'a [T],
}
impl<'a, const N: usize, T> Iterator for ArrayChunks<'a, N, T> {
    type Item = &'a [T; N];
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining.len() >= N {
            let (chunk, remaining) = self.remaining.split_at(N);
            self.remaining = remaining;
            chunk.try_into().ok()
        } else {
            None
        }
    }
}
pub fn array_chunks<const N: usize, T>(array: &[T]) -> ArrayChunks<N, T> {
    ArrayChunks { remaining: array }
}

pub struct IterChunks<const N: usize, I> {
    iter: I,
}
impl<const N: usize, I> Iterator for IterChunks<N, I>
where
    I: Iterator,
{
    type Item = [<I as Iterator>::Item; N];
    fn next(&mut self) -> Option<Self::Item> {
        let mut vec = Vec::with_capacity(N);
        for _ in 0..N {
            vec.push(self.iter.next()?);
        }
        vec.try_into().ok()
    }
}
pub fn iter_chunks<const N: usize, I>(iter: I) -> IterChunks<N, I> {
    IterChunks { iter }
}
