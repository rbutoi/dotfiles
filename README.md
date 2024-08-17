### Install

With [`dotfiler`](https://github.com/svetlyak40wt/dotfiler) (https://github.com/svetlyak40wt/dotfiler):

```
git clone https://github.com/svetlyak40wt/dotfiler ~/.dots

python3 ~/.dots/bin/dot add git@github.com:rbutoi/dotfiles.git
python3 ~/.dots/bin/dot add git@gitlab.com:rbutoi/private-dots.git
python3 ~/.dots/bin/dot update

# optional, so that rg & fd searchs of all dotfiles work
rm ~/.dots/.gitignore
```
