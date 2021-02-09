#!/bin/zsh
rm -R ./bin
meson setup bin
ninja -C bin


# EOF
