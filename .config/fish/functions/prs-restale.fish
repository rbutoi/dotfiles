function prs-restale --description 'My open PRs whose approvals were dismissed (need re-approval)'
    # OSC-8 hyperlinks only render on a terminal; when piped, emit a plain URL
    # column instead so the escape bytes stay out of the output.
    set -l tty false
    isatty stdout && set tty true

    begin
        if test $tty = true
            printf 'PR\tLOST APPROVALS FROM\tTITLE\n'
        else
            printf 'PR\tLOST APPROVALS FROM\tURL\tTITLE\n'
        end
        # $E is ESC, $ST is the ESC-backslash string terminator, both built via
        # implode so no backslash needs escaping through fish and jq. ST is
        # required here: ghostty ignores the BEL-terminated OSC-8 form (eza,
        # which works, uses ST too). That also rules out @tsv, which would
        # escape ST's backslash -- hence join("\t").
        # The linked title stays last so its non-printing bytes don't throw off
        # column(1)'s width math.
        gh api graphql -f query='{viewer{pullRequests(states:OPEN,first:50,orderBy:{field:UPDATED_AT,direction:DESC}){nodes{
        url title number reviewDecision repository{nameWithOwner}
        timelineItems(itemTypes:[REVIEW_DISMISSED_EVENT],first:20){
          nodes{...on ReviewDismissedEvent{previousReviewState review{author{login}}}}}}}}}' \
            | jq -r --argjson tty $tty '
            ([27]|implode) as $E | ([27,92]|implode) as $ST
            | .data.viewer.pullRequests.nodes[]
            | select(.reviewDecision != "APPROVED")
            | [.timelineItems.nodes[]
               | select(.previousReviewState == "APPROVED")
               | .review.author.login // "?"] as $who
            | select(($who | length) > 0)
            | ["\(.repository.nameWithOwner)#\(.number)", ($who | unique | join(", "))]
              + (if $tty
                 then ["\($E)]8;;\(.url)\($ST)\(.title)\($E)]8;;\($ST)"]
                 else [.url, .title] end)
            | join("\t")'
    end | column -ts \t
end
