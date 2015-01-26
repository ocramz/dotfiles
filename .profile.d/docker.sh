#-*- mode: sh -*-

function docker-gc {
    for i in $(docker images|grep \<none\>|cut -d' ' -f35); do
        docker rmi $i
    done
    unset i
}
