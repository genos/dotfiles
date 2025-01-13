function gi -d "Generate .gitignore file"
  curl -sL https://www.toptal.com/developers/gitignore/api/$argv[1]
end
