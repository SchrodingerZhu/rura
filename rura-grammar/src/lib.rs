use winnow::PResult;

pub fn file<'s>(_: &mut &'s str) -> PResult<&'s str> {
    Ok("")
}

#[cfg(test)]
mod tests;
