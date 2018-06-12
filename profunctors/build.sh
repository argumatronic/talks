#! /usr/bin/env bash

nix-shell \
  --packages 'texlive.combine { inherit (texlive) scheme-medium beamer beamertheme-metropolis pgfopts upquote; }' \
  --run 'pandoc -t beamer --pdf-engine=xelatex -V theme=metropolis day1.md -o day1.pdf'

nix-shell \
  --packages 'texlive.combine { inherit (texlive) scheme-medium beamer beamertheme-metropolis pgfopts upquote; }' \
  --run 'pandoc -t beamer --pdf-engine=xelatex -V theme=metropolis day2.md -o day2.pdf'

