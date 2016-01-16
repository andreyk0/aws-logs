aws-logs
========

    Tail-like utility for Cloud Watch Logs.

    Usage: aws-logs [-r|--region ARG] (-g|--log-group-name ARG)
                    [-s|--log-stream-name ARG] [-F|--output-format ARG]
                    [-S|--start-time ARG] [-E|--end-time ARG] [-f|--follow]
                    [-v|--verbose] [CWL filter pattern]

    Available options:
      -h,--help                Show this help text
      -r,--region ARG          AWS Region (default: "us-east-1")
      -g,--log-group-name ARG  CWL log group name
      -s,--log-stream-name ARG CWL log stream name, can be given multiple times
      -F,--output-format ARG   Output format [text|json] (default: "text")
      -S,--start-time ARG      Start time (default: "2015-10-24T00:57:26UTC")
      -E,--end-time ARG        End time
      -f,--follow              Keep looking for more results. N.B. this can skip
                               over some events.
      -v,--verbose             Run in a verbose mode, for debugging
      CWL filter pattern       E.g. '"some-pattern"'
                               http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/FilterAndPatternSyntax.html

    E.g. aws-logs -g my-log-group '"da240fb2-9b9d-4b80-bac2-f3452e937919"'
    
    
Run binary
============

Binary is published to a [Docker hub repo](https://hub.docker.com/r/andreyk0/aws-logs/), it can be executed with [this script](aws-logs).
