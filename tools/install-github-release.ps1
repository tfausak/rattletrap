if (Test-Path c:/bin/github-release.exe) {} else {
  curl -OutFile github-release.zip -Uri https://github.com/tfausak/github-release/releases/download/1.2.1/github-release-windows.zip
  7z x github-release.zip github-release.exe
  mkdir c:/bin
  mv github-release.exe c:/bin
}

github-release version
