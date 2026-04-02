function brew_wait --description 'Wait for any running brew processes to finish'
    wait_proc 'Homebrew/brew.rb' -m brew
end
