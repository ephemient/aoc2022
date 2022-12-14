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

pub struct IterPairs<I: Iterator> {
    prev: Option<<I as Iterator>::Item>,
    iter: I,
}
impl<I> Iterator for IterPairs<I>
where
    I: Iterator,
    <I as Iterator>::Item: Clone,
{
    type Item = (<I as Iterator>::Item, <I as Iterator>::Item);
    fn next(&mut self) -> Option<Self::Item> {
        let prev = self.prev.take().or_else(|| self.iter.next())?;
        let next = self.iter.next()?;
        self.prev = Some(next.clone());
        Some((prev, next))
    }
}
pub fn iter_pairs<I: Iterator>(iter: I) -> IterPairs<I> {
    IterPairs { prev: None, iter }
}
