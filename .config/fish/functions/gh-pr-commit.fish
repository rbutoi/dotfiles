function gh-pr-commit --description "Print the first-parent commit SHA for a merged PR number"
    git log --first-parent --grep="(#$argv)" --format=%H -1
end
