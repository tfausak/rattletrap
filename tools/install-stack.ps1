if (Test-Path c:\bin\stack.exe) {} else {
  curl -OutFile stack.zip -Uri https://www.stackage.org/stack/windows-x86_64
  7z x stack.zip stack.exe
  mkdir c:\bin
  mv stack.exe c:\bin
}

stack --version
