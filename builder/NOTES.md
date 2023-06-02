# Notes

I didn't find a way to generate the expected error in test 08
using stable Rust 1.69.0, meanwhile the current implementation
generates it on nightly-2023-05-23.

See comments in `src/lib.rs` for more.

It's easy to generate an error pointing to only the "eac".
