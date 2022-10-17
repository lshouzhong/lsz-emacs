# Install elisp files

## Debian / Ubuntu

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

## Windows

Clone this repo to `C:\Users\<username>\AppData\Roaming\lsz-emacs` and initialize submodules.

Put the content below to `~/.emacs.d/init.el` ( equals to `C:\Users\<username>\AppData\Roaming\.emacs.d\init.el` ).

```lisp
(load-file "~/lsz-emacs/site-start.el")
```

# Install dependencies for extensions

## Debian / Ubuntu

```plaintext
## for counsel-rg
sudo apt install ripgrep

## lsp-bridge python dependences
sudo apt install python3-pip
pip install epc orjson

## lsp-bridge python lsp server
pip install pyright

## lsp-bridge npm packages
# install npm manually
npm i -g pyright # for python
npm i -g typescript typescript-language-server # for js/ts
npm i -g vscode-langservers-extracted # for HTML/CSS/JSON/ESLint 
npm i -g yaml-language-server # for yaml

## emacs-rime
## use ibus-rime in gnome
## note: opencc available, lua extension unavailable
sudo apt install librime-dev
sudo apt install ibus-rime
```

Note: Emacs will load ~/.profile for env variables at the start. According to the code in ~/.profile, emacs will not load ~/.bashrc. So if user wish to add some env variables, the code should be written to ~/.profile.

## Windows

Install nodejs and python. Then install the npm and python packages previous section listed.

Download the ripgrep for windows from [ripgrep](https://github.com/BurntSushi/ripgrep) and add the .exe file to env variable `Path`.

For emacs-rime, see [Weasel](https://github.com/rime/weasel). Then deploy you own rime configs. But if you use liberime/librime.dll for librime-emacs lib, opencc and lua extension are unavailable.
