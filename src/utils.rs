#![allow(unused)]

use std::fmt::Debug;

pub struct StreamTryMap<S, F>(S, F);

pub trait StreamTryMapEx: futures_util::TryStream {
    fn try_map<F, T>(self, f: F) -> StreamTryMap<Self, F>
    where
        F: FnMut(Self::Ok) -> T,
        Self: Sized,
    {
        StreamTryMap(self, f)
    }
}
impl<T: futures_util::TryStream> StreamTryMapEx for T {}

impl<S, F, Ok, Err, T> futures_util::Stream for StreamTryMap<S, F>
where
    S: futures_util::Stream<Item = Result<Ok, Err>> + Unpin,
    F: FnMut(Ok) -> T,
{
    type Item = Result<T, Err>;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        use std::task::Poll;
        let stream = unsafe { &mut self.get_unchecked_mut() };
        match std::pin::Pin::new(&mut stream.0).poll_next(cx) {
            Poll::Ready(Some(Ok(value))) => Poll::Ready(Some(Ok((stream.1)(value)))),
            Poll::Ready(Some(Err(error))) => Poll::Ready(Some(Err(error))),
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// helper struct to collect something into a first part which does not repeat and a second part which repeats
pub struct GroupBy<Col, Key, ValCol>(Col, Option<(Key, ValCol)>);

impl<Col, Key, ValCol> GroupBy<Col, Key, ValCol> {
    pub fn into_all(mut self) -> Col
    where
        Col: Extend<(Key, ValCol)>,
    {
        self.0.extend(self.1);
        self.0
    }
    pub fn into_one(self) -> Option<(Key, ValCol)>
    where
        Col: IntoIterator,
    {
        if self.0.into_iter().next().is_some() {
            panic!("GroupBy::into_one: many found");
        }
        self.1
    }
}
impl<Col, Key, ValCol> Default for GroupBy<Col, Key, ValCol>
where
    Col: Default,
{
    fn default() -> Self {
        Self(Col::default(), None)
    }
}
impl<Col, Key, Val, ValCol, ValIter> Extend<(Key, ValIter)> for GroupBy<Col, Key, ValCol>
where
    Key: PartialEq + Debug,
    Col: Extend<(Key, ValCol)>,
    ValCol: Extend<Val> + Default,
    ValIter: IntoIterator<Item = Val>,
{
    fn extend<T: IntoIterator<Item = (Key, ValIter)>>(&mut self, iter: T) {
        for (key, val_iter) in iter {
            if let Some((cur_key, cur_val_col)) = &mut self.1 {
                if *cur_key == key {
                    cur_val_col.extend(val_iter);
                } else {
                    let old_key = std::mem::replace(cur_key, key);
                    let old_val_col = std::mem::take(cur_val_col);
                    self.0.extend(std::iter::once((old_key, old_val_col)));
                    cur_val_col.extend(val_iter);
                }
            } else {
                let (_, cur_val_col) = self.1.get_or_insert((key, ValCol::default()));
                cur_val_col.extend(val_iter);
            }
        }
    }
}
