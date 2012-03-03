#-*- mode: sh -*-

# linux

if [ -f /proc/cpuinfo ]; then

    if [ -z $SSH_AGENT_PID ]; then
        eval `ssh-agent` >/dev/null
    fi

    if [ -z $DBUS_SESSION_BUS_ADDRESS ]; then
        eval `dbus-launch`; export DBUS_SESSION_BUS_ADDRESS
    fi

fi

# archlinux

if [ -f /etc/arch-release ]; then

    export JAVA_HOME=/usr/lib/jvm/java-7-openjdk

fi

# mac

if [ -d /Applications ]; then

    export JAVA_HOME=$(/usr/libexec/java_home)

    find-tag() { each $1 openmeta -p $(eval 'pwd') -a $2 }

    finder-hide-files() {
        defaults write com.apple.finder AppleShowAllFiles $1 ;# TRUE | FALSE
        killall Finder
    }

fi
