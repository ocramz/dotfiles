#-*- mode: sh -*-

if [ -d /usr/local/Library/LinkedKegs ]; then
    export AWS_AUTO_SCALING_HOME="/usr/local/Library/LinkedKegs/auto-scaling/jars"
    export AWS_CLOUDFORMATION_HOME="/usr/local/Library/LinkedKegs/aws-cfn-tools/jars"
    export AWS_CLOUDWATCH_HOME="/usr/local/Library/LinkedKegs/cloud-watch/jars"
    export AWS_ELASTICACHE_HOME="/usr/local/Library/LinkedKegs/aws-elasticache/jars"
    export AWS_ELB_HOME="/usr/local/Library/LinkedKegs/elb-tools/jars"
    export AWS_IAM_HOME="/usr/local/Library/LinkedKegs/aws-iam-tools/jars"
    export AWS_SNS_HOME="/usr/local/Library/LinkedKegs/aws-sns-cli/jars"
    export CS_HOME="/usr/local/Library/LinkedKegs/aws-cloudsearch/jars"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"
fi
