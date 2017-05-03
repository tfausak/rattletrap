if (Test-Path c:\bin\stack.exe) {} else {
  curl -OutFile stack.zip -Uri https://github.com/commercialhaskell/stack/releases/download/v1.4.0/stack-1.4.0-windows-x86_64.zip
  7z x stack.zip stack.exe
  mkdir c:\bin
  mv stack.exe c:\bin
}

stack --version
