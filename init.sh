#! /bin/bash

SCRIPT_DIR=$(cd $(dirname $0); pwd)

echo start

if [ ! -e $HOME/.bash_addon ]; then
  ln -s $PWD/.bash_addon $HOME/.bash_addon
  echo 'copy .bash_addon'
fi

if [ ! -d $HOME/.config ]; then
  ln -s $PWD/.config $HOME/.config
  echo 'copy .config'
fi

if [ ! -e $HOME/.pryrc ]; then
  ln -s $PWD/.pryrc $HOME/.pryrc
  echo 'copy .pryrc'
fi

if [ ! -e $HOME/.gitignore ]; then
  ln -s $PWD/.gitignore $HOME/.gitignore
  echo 'copy .gitignore'
fi

echo finish
