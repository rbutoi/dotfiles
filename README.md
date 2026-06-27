### Install

With [`dotfiler`](https://github.com/svetlyak40wt/dotfiler) (https://github.com/svetlyak40wt/dotfiler):

```bash
git clone https://github.com/svetlyak40wt/dotfiler ~/.dots

# make sure automatically-written files aren't in source control
for dir in \
    ~/.config/{emacs,fish/{conf.d,functions},zed}; do

    if [[ -L $dir ]]; then
        rm -i $dir
    fi
    mkdir -p $dir
done

python3 ~/.dots/bin/dot add git@github.com:rbutoi/dotfiles.git
python3 ~/.dots/bin/dot add git@gitlab.com:rbutoi/private-dots.git
python3 ~/.dots/bin/dot update
```
