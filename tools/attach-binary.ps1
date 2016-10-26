if ($env:APPVEYOR_REPO_TAG_NAME) {
  if ($env:GITHUB_TOKEN) {
    stack --local-bin-path . install rattletrap
    7z a rattletrap.zip rattletrap.exe
    github-release upload --token "$env:GITHUB_TOKEN" --owner tfausak --repo rattletrap --tag "$env:APPVEYOR_REPO_TAG_NAME" --file rattletrap.zip --name "rattletrap-$env:APPVEYOR_REPO_TAG_NAME-windows.zip"
  }
}
