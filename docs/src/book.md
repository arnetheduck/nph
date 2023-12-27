# Updating this book

The book is built using [mdBook](https://github.com/rust-lang/mdBook), and published to gh-pages using a github action.

```bash
# Install or update tooling (make sure you add "~/.cargo/bin" to PATH):
cargo install mdbook --version 0.4.36
cargo install mdbook-toc --version 0.14.1
cargo install mdbook-open-on-gh --version 2.4.1
cargo install mdbook-admonish --version 1.14.0

# Edit book and view through local browser
mdbook serve
```
