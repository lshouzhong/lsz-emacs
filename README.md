## Install elisp files

### Debian / Ubuntu

Clone this repo to home path.

Build config symlink to emacs directory:

```bash
sudo ln -s /home/<username>/lsz-emacs/site-lisp /usr/share/emacs/lsz
```

Copy site-start.el in emacs directory to start my config:

```bash
sudo cp /home/<username>/lsz-emacs/site-start.el /usr/share/emacs/site-lisp/
```

Initialize submodules:

```plaintext
cd ~/lsz-emacs && git submodule update --init --recursive
```

### Windows

Clone this repo to `C:\Users\<username>\AppData\Roaming\lsz-emacs` and initialize submodules.

Put the content below to `~/.emacs.d/init.el` ( equals to `C:\Users\<username>\AppData\Roaming\.emacs.d\init.el` ).

```lisp
(load-file "~/lsz-emacs/site-start.el")
```

## Install dependencies for extensions

### Debian / Ubuntu

```plaintext
# color-rg
sudo apt install ripgrep

# lsp-bridge python dependences
pip install epc orjson

# lsp-bridge python lsp server
pip install pyright

# emacs-rime
# usually I use ibus-rime in gnome
# opencc available, lua extension unavailable
sudo apt install librime-dev
sudo apt install ibus-rime
```

### Windows

Download the python packages previous section listed.

Download the ripgrep for windows from [ripgrep](https://github.com/BurntSushi/ripgrep) and add the .exe file to env variable `Path`.

For emacs-rime, see [Weasel](https://github.com/rime/weasel). Then deploy you own rime configs. But if you use liberime/librime.dll for librime-emacs lib, opencc and lua extension are unavailable.
