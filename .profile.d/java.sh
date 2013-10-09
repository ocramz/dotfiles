#-*- mode: sh -*-

if [ -f /usr/libexec/java_home ]; then # MAC
    export JAVA_HOME="$(/usr/libexec/java_home -v 1.6+)"
fi

export MAVEN_OPTS="-Xmx512m -XX:MaxPermSize=128m"
