  curl -OutFile stack.zip -Uri https://github.com/commercialhaskell/stack/releases/download/v1.9.3/stack-1.9.3-windows-x86_64.zip
  7z x stack.zip stack.exe
  mkdir c:/bin
  mv stack.exe c:/bin

stack --version
