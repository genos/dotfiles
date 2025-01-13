function check_writing -d "Run the plaintext-ish file through various writing checks"
  weasel $argv[1] && passive $argv[1] && dups $argv[1] && alex $argv[1]
end
