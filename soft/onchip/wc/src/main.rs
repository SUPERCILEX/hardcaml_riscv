#![feature(maybe_uninit_slice)]
#![no_std]
#![no_main]

use core::{mem::MaybeUninit, str::from_utf8};

use slib::{println, stdio::read_byte};

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(_: &[&str]) -> i8 {
    let mut text = [MaybeUninit::uninit(); 4096];
    let text = load_bytes(&mut text);

    println!("{:?}", words_count::count(from_utf8(text).unwrap()));

    0
}

fn load_bytes(into: &mut [MaybeUninit<u8>]) -> &mut [u8] {
    let mut count = 0;
    for elem in &mut *into {
        let byte = read_byte();
        if byte == 4 {
            break;
        }

        elem.write(byte);
        count += 1;
    }
    unsafe { MaybeUninit::slice_assume_init_mut(&mut into[..count]) }
}

pub mod words_count {
    /*!
    # Words Count

    Count the words and characters, with or without whitespaces.

    The algorithm is roughly aligned with the way LibreOffice is counting words. This means that it does not exactly match the [Unicode Text Segmentation](https://unicode.org/reports/tr29/#Word_Boundaries) standard.

    ## Examples

    ```rust
    use words_count::WordsCount;

    assert_eq!(WordsCount {
        words: 20,
        characters: 31,
        whitespaces: 2,
        cjk: 18,
    }, words_count::count("Rust是由 Mozilla 主導開發的通用、編譯型程式語言。"));
    ```

    ```rust
    let result = words_count::count_separately("apple banana apple");

    assert_eq!(2, result.len());
    assert_eq!(Some(&2), result.get("apple"));
    ```
     */

    use core::{
        ops::{Add, AddAssign},
        str::from_utf8_unchecked,
    };

    use heapless::FnvIndexMap;

    #[derive(Debug, Clone, Default, Eq, PartialEq)]
    pub struct WordsCount {
        pub words: usize,
        pub characters: usize,
        pub whitespaces: usize,
        pub cjk: usize,
    }

    /// A WordsCount equivalent to words_count::count("\n").
    ///
    /// It is useful when processing files a line at a time.
    ///
    /// ## Example
    ///
    /// ```rust
    /// use words_count::{count, WordsCount, NEWLINE};
    ///
    /// let mut total = WordsCount::default();
    /// for ln in std::io::stdin().lines() {
    ///     total += count(ln.unwrap()) + NEWLINE;
    /// }
    /// println!("{total:?}");
    /// ```
    pub const NEWLINE: WordsCount = WordsCount {
        words: 0,
        characters: 1,
        whitespaces: 1,
        cjk: 0,
    };

    impl AddAssign for WordsCount {
        #[inline]
        fn add_assign(&mut self, other: Self) {
            *self = Self {
                words: self.words + other.words,
                characters: self.characters + other.characters,
                whitespaces: self.whitespaces + other.whitespaces,
                cjk: self.cjk + other.cjk,
            }
        }
    }

    impl Add for WordsCount {
        type Output = Self;

        #[inline]
        fn add(mut self, other: Self) -> Self {
            self += other;
            self
        }
    }

    /// Count the words in the given string. In general, every non-CJK string of
    /// characters between two whitespaces is a word. Dashes (at least two
    /// dashes) are word limit, too. A CJK character is considered to be an
    /// independent word.
    pub fn count<S: AsRef<str>>(s: S) -> WordsCount {
        let mut in_word = false;
        let mut consecutive_dashes = 0usize;

        let mut count = WordsCount::default();

        for c in s.as_ref().chars() {
            count.characters += 1;

            if c.is_whitespace() {
                consecutive_dashes = 0;

                count.whitespaces += 1;

                if in_word {
                    count.words += 1;

                    in_word = false;
                }
            } else {
                match c {
                    '-' => {
                        consecutive_dashes += 1;

                        if consecutive_dashes > 1 && in_word {
                            if consecutive_dashes == 2 {
                                count.words += 1;
                            }

                            in_word = false;

                            continue;
                        }
                    }
                    _ => {
                        consecutive_dashes = 0;

                        if unicode_blocks::is_cjk(c) {
                            count.words += 1;
                            count.cjk += 1;

                            if in_word {
                                count.words += 1;

                                in_word = false;
                            }

                            continue;
                        }
                    }
                }

                if !in_word {
                    in_word = true;
                }
            }
        }

        if in_word {
            count.words += 1;
        }

        count
    }

    /// Count the words separately in the given string. In general, every
    /// non-CJK string of characters between two whitespaces is a word. Dashes
    /// (at least two dashes) are word limit, too. A CJK character is considered
    /// to be an independent word. Punctuations are not handled.
    pub fn count_separately<S: ?Sized + AsRef<str>>(s: &S) -> FnvIndexMap<&str, usize, 1024> {
        let mut in_word = false;
        let mut consecutive_dashes = 0usize;

        let mut count = FnvIndexMap::new();

        let mut p = 0;
        let mut pp = 0;

        let s = s.as_ref();
        let bytes = s.as_bytes();

        for c in s.chars() {
            let cl = c.len_utf8();

            if c.is_whitespace() {
                if in_word {
                    inc_or_insert(&mut count, unsafe { from_utf8_unchecked(&bytes[p..pp]) });

                    in_word = false;
                }

                p = pp + cl;

                consecutive_dashes = 0;
            } else {
                match c {
                    '-' => {
                        consecutive_dashes += 1;

                        if consecutive_dashes > 1 {
                            if in_word {
                                if consecutive_dashes == 2 {
                                    inc_or_insert(&mut count, unsafe {
                                        from_utf8_unchecked(&bytes[p..(pp - 1)])
                                    });
                                }

                                in_word = false;

                                pp += cl;
                                p = pp;
                                continue;
                            } else {
                                p = pp + cl;
                            }
                        }
                    }
                    _ => {
                        if unicode_blocks::is_cjk(c) {
                            inc_or_insert(&mut count, unsafe {
                                from_utf8_unchecked(&bytes[pp..(pp + cl)])
                            });

                            if in_word {
                                inc_or_insert(&mut count, unsafe {
                                    from_utf8_unchecked(&bytes[p..pp])
                                });

                                in_word = false;
                            }

                            consecutive_dashes = 0;
                            pp += cl;
                            p = pp;
                            continue;
                        }

                        consecutive_dashes = 0;
                    }
                }

                if !in_word {
                    in_word = true;
                }
            }

            pp += cl;
        }

        if in_word {
            inc_or_insert(&mut count, unsafe { from_utf8_unchecked(&bytes[p..pp]) });
        }

        count
    }

    #[inline]
    fn inc_or_insert<'a>(map: &mut FnvIndexMap<&'a str, usize, 1024>, s: &'a str) {
        if let Some(count) = map.get_mut(s) {
            *count += 1;
        } else {
            map.insert(s, 1).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use core::{arch::global_asm, hint::black_box};

    use crate::main;

    global_asm!(
        r#"
        .pushsection .start, "ax"
        .globl _start
        _start:
            li sp, 0x80000000
            j {}
        .popsection
        "#,
        sym runtime
    );

    fn runtime() {
        test(&[]);
    }

    fn test(args: &[&str]) {
        main(black_box(args));
    }
}
