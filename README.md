aws-logs
========

```
Tail-like utility for Cloud Watch Logs.

Usage: aws-logs [-r|--region ARG] [-p|--profile ARG] (-g|--log-group-name ARG)
                [-s|--log-stream-name ARG] [-F|--output-format ARG]
                [-S|--start-time ARG] [-E|--end-time ARG] [-f|--follow]
                [-d|--follow-delay-seconds ARG] [-v|--verbose]
                [-m|--include-event-metadata] [CWL filter pattern]

Available options:
  -h,--help                Show this help text
  -r,--region ARG          AWS Region (default: "us-east-1")
  -p,--profile ARG         AWS Profile
  -g,--log-group-name ARG  CWL log group name
  -s,--log-stream-name ARG CWL log stream name, can be given multiple times
  -F,--output-format ARG   Output format [text|json] (default: "text")
  -S,--start-time ARG      Start time (default: "2016-10-21T23:37:06UTC")
  -E,--end-time ARG        End time
  -f,--follow              Keep looking for more results. N.B. this can skip
                           over some events
  -d,--follow-delay-seconds ARG
                           How many seconds to delay output by in the 'follow'
                           mode (default: 10)
  -v,--verbose             Run in a verbose mode, for debugging
  -m,--include-event-metadata
                           Include event metadata in the output
  CWL filter pattern       E.g. '"some-pattern"'
                           http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/FilterAndPatternSyntax.html

E.g. aws-logs -g my-log-group '"da240fb2-9b9d-4b80-bac2-f3452e937919"' OR
aws-logs -g my-log-group '{$.level = "ERROR"}'
```
