pub type Either<A, B> = Result<B, A>;

pub enum Unsupported {
    Unsupported,
}
