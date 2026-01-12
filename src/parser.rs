use crate::tokens::Token;

pub struct Parser<'src> {
    tokens: &'src Vec<Token<'src>>,
    curr: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src Vec<Token<'src>>) -> Self {
        Self { tokens, curr: 0 }
    }

    fn primary(&self) {
        todo!();
    }

    fn unary(&self) {
        todo!();
    }

    fn factor(&self) {
        todo!();
    }

    fn term(&self) {
        todo!();
    }

    fn expr(&self) {
        todo!();
    }

    pub fn parse(&self) {
        let ast = self.expr();
    }
}
