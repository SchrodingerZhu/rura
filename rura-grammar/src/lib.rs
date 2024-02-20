use winnow::PResult;

pub fn file<'s>(input: &mut &'s str) -> PResult<&'s str> {
    Ok("")
}

#[cfg(test)]
mod tests;
