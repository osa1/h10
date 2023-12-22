const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const LOW_SIX_BITS_MASK: usize = 0b0011_1111;

pub fn base64_encode(mut i: usize) -> String {
    let alphabet = ALPHABET.as_bytes();

    // A character in base64 encodes 6 bits.
    let encoding_size = (std::mem::size_of::<usize>() * 8).div_ceil(6);

    let mut vec = vec![0; encoding_size];

    for byte in &mut vec {
        let idx = i & LOW_SIX_BITS_MASK;
        *byte = alphabet[idx];
        i >>= 6;
    }

    String::from_utf8(vec).unwrap()
}
