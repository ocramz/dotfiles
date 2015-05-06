#-*- mode: sh -*-

function jq-format() {
    (cat $1 | jq -S . | tee tmp.json) && mv tmp.json $1
}
