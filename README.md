### Install

Managed by [my fork of `dotfiler`](https://github.com/svetlyak40wt/dotfiler),
which lives in `~/dev/dotfiler` alongside everything else. Environments are
declared in config rather than having to sit inside the tool's own checkout, so
this repo is just a repo:

```fish
git clone <this repo>         ~/dev/dotfiles
git clone <the private one>   ~/dev/private-dots
git clone <the dotfiler fork> ~/dev/dotfiler

# Directories that Emacs, fisher and Zed write into must be real directories, so
# that `dot` links individual files into them instead of symlinking the whole
# directory. Without this it symlinks the directory and those tools then write
# into the repo.
#
# ~/.claude/skills is here for a different reason: both environments contribute
# skills to it, and a whole-directory symlink can only point at one of them. As a
# real directory it gets per-entry links, so the public generic skills and the
# private work-specific ones (which name internal channels and repos) merge in
# ~/.claude/skills without the private ones landing in this public repo.
for dir in ~/.config/{emacs,fish/{conf.d,functions},zed} ~/.claude/skills
    test -L $dir && rm -i $dir
    mkdir -p $dir
end

# One bootstrap run with explicit paths: it creates ~/.config/dotfiler/config.toml
# (tracked here), after which plain `dot update` reads the config itself.
~/dev/dotfiler/bin/dot update --env ~/dev/dotfiles --env ~/dev/private-dots
```

Afterwards, `dotup` (see `.config/fish/config.fish`) is `dot update --skip-pull`.

Background git fetch and maintenance is a separate opt-in per machine; see the
header of `.config/git-maintenance/config.toml`.
