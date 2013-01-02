#-*- mode: sh -*-

if [ -d ~/bin ]; then
    PATH=${HOME}/bin:${PATH}
fi

export EDITOR='emacs -nw'
export GIT_EDITOR=nano
export PAGER=less

# NIX
if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
    source ~/.nix-profile/etc/profile.d/nix.sh
fi

# HASKELL
if [ -d ${HOME}/.cabal/bin ]; then
    PATH=${HOME}/.cabal/bin:${PATH}
fi

# GO
export GOROOT=/usr/local/go

# PYTHON
export VIRTUALENV_DISTRIBUTE=true

# RUBY
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# OCAML
which opam >/dev/null && eval `opam config -env`

# MAC
if [ -f /mach_kernel ]; then
    # JAVA
    export JAVA_HOME="$(/usr/libexec/java_home)"

    # PYTHON
    export PYTHONPATH=${HOME}/Library/Python/2.7/site-packages

    # AWS
    export AWS_AUTO_SCALING_HOME="/usr/local/Library/LinkedKegs/auto-scaling/jars"
    export AWS_CLOUDFORMATION_HOME="/usr/local/Library/LinkedKegs/aws-cfn-tools/jars"
    export AWS_CLOUDWATCH_HOME="/usr/local/Library/LinkedKegs/cloud-watch/jars"
    export AWS_ELASTICACHE_HOME="/usr/local/Library/LinkedKegs/aws-elasticache/jars"
    export AWS_IAM_HOME="/usr/local/Library/LinkedKegs/aws-iam-tools/jars"
    export AWS_SNS_HOME="/usr/local/Library/LinkedKegs/aws-sns-cli/jars"
    export CS_HOME="/usr/local/Library/LinkedKegs/aws-cloudsearch/jars"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"
fi
