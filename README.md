# Emacs-Kick(starter) for Vim/Neovim Users

Welcome to `Emacs-Kick`, a feature-rich Emacs configuration designed
for users familiar with `Vim`, `Neovim`, and `Vi`. With this
setup, you don't need to leave behind your favorite terminal or tools
like `yazi`, `starship`, `lazygit`, and `lazydocker` just to
give Emacs a try.

Use Emacs the same way you'd use `Neovim`, seamlessly integrating it
into your workflow inside terminal multiplexers like `tmux` or
`Zellij`, while also enjoying modern features such as `treesitter`
and `LSP`—no hassle.

TODO: [Insert nice screenshot here]

`Emacs-Kick` is not a distribution, but a starting point for your
own configuration. It’s designed to be accessible to Vim/Neovim users
without needing to adopt all of Emacs' ecosystem. You can still enjoy
the power of Emacs without having to learn every Emacs-specific
workflow.

**Minimum Requirements**:
- Emacs version **29+**
  - You can verify your version by running:

```bash
emacs --version
```

**Installation Instructions**:

1. **Clone the repository**:

   **Note**: If you already have an existing Emacs configuration in
   `~/.emacs.d`, please back it up before proceeding. You can do this
   by renaming the directory:

```bash
mv ~/.emacs.d ~/.emacs.d.backup
```

   After backing up, clone the repository:

```bash
git clone https://github.com/LionyxML/emacs-kick.git ~/.emacs.d
```

2. **Run the setup**:

   After cloning, install the configuration by running:

```bash
emacs -nw --eval="(ek/first-install)"
```

   Alternatively, you can run the provided script `ek-reinstall.sh`
   from inside `~/.emacs.d/`, which will achieve the same result:

```bash
cd ~/.emacs.d/ && ./ek-reinstall.sh
```

   Both methods will install all necessary packages and apply the
   configuration.

3. **Set terminal mode by default**:

   **Note on Emacs modes**: Emacs automatically adapts to either
   graphical or terminal mode depending on the environment. But if
   you're in a graphical session and prefer terminal mode, just use:

```bash
emacs -nw
```

   To ensure Emacs always opens in terminal mode, add the following to
   your `.bashrc` or `.zshrc`:

```bash
alias emacs='emacs -nw'
```

   Then, reload your shell configuration with:

```bash
source ~/.bashrc  # for bash
source ~/.zshrc   # for zsh
```

4. **Start Emacs**:

   Once set up, start Emacs with:

```bash
emacs
```

**Usage Tips**:
- **Leader Key**: The leader key is set to `SPC` (spacebar), similar
  to Vim.
- **Help Commands**:
  - `SPC h i` opens the Emacs info documentation (`M-x info`).
  - `SPC h v` allows you to explore available variables.
  - `SPC h f` lets you explore functions.
  - `SPC h k` displays keybindings.

**Troubleshooting**:
- If you encounter any issues during installation, check the
  `*Messages*` buffer for more information. You can switch between
  buffers with `SPC SPC`, and navigate options using `C-p` and `C-n`.

---

This configuration is tailored for Vim/Neovim users looking to explore
Emacs while keeping their existing workflow intact. Feel free to
explore and customize it further as you go.

Enjoy your Emacs experience!

# Contributing

This package is intentionally designed with a specific vision in mind,
reflecting my own opinions and preferences. While contributions are
welcome, please understand that this configuration is quite
opinionated.

If you have suggestions or requests, they will be considered
carefully, but I cannot make any promises regarding implementation or
acceptance. Your input is valuable, and I appreciate any help or
feedback to improve the project.

To contribute, feel free to open an issue or submit a pull
request. Let's make this configuration even better together!
