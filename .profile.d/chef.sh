#-*- mode: sh -*-

function chef-search {
    knife exec -E "print nodes.find('$1').collect {|n| n.ec2.public_hostname}.join ' '"
}

function chef-ssh {
    query=$1; shift; pssh -l tim -H "$(chef-search $query)" $@
}

alias knife='nocorrect knife'
# alias k=knife
