# aws-logs (Query AWS CloudWatch logs from the command line)


# Install

```bash
curl -L https://raw.githubusercontent.com/andreyk0/aws-logs/master/install | /bin/bash
```

# CLI

```
Tail-like utility for Cloud Watch Logs.

Usage: aws-logs ((-V|--version) | [-v|--verbose] [-r|--region ARG]
                [-p|--profile ARG] ((-L|--list-streams log-group-name) |
                (-l|--list-log-groups) | (-g|--log-group-name ARG)
                [-s|--log-stream-name ARG] [-F|--output-format ARG]
                [-S|--start-time ARG] [-E|--end-time ARG] [-f|--follow]
                [-d|--follow-delay-seconds ARG] [-m|--include-event-metadata]
                [CWL filter pattern]))

Available options:
  -h,--help                Show this help text
  -V,--version             Print version and exit.
  -v,--verbose             Run in a verbose mode, for debugging
  -r,--region ARG          AWS Region (default: "us-east-1")
  -p,--profile ARG         AWS Profile
  -L,--list-streams log-group-name
                           List all available streams in a log group.
  -l,--list-log-groups     List all available log groups.
  -g,--log-group-name ARG  CWL log group name
  -s,--log-stream-name ARG CWL log stream name, can be given multiple times
  -F,--output-format ARG   Output format [text|json] (default: text)
  -S,--start-time ARG      Start time (default: 2017-05-06T15:16:49UTC)
  -E,--end-time ARG        End time
  -f,--follow              Keep looking for more results. N.B. this can skip
                           over some events
  -d,--follow-delay-seconds ARG
                           How many seconds to delay output by in the 'follow'
                           mode (default: 3)
  -m,--include-event-metadata
                           Include event metadata in the output
  CWL filter pattern       E.g. '"some-pattern"'

Filter pattern syntax documentation: http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/FilterAndPatternSyntax.html

Search for a UUID:
  aws-logs -g my-log-group '"da240fb2-9b9d-4b80-bac2-f3452e937919"'

Search JSON logs for ERRORs:
  aws-logs -g my-log-group '{$.level = "ERROR"}'

List available log groups:
  aws-logs -l

List available streams in a log group:
  aws-logs -L my-log-group

For JSON filtering/formatting please pipe output to 'jq' (https://stedolan.github.io/jq/manual/)

Start/end time parameters support these formats: ["%Y-%m-%dT%H:%M:%S%Z","%Y-%m-%dT%H:%M%Z","%Y-%m-%dT%H%Z","%Y-%m-%d%Z","%Y-%m-%dT%H:%M:%S","%Y-%m-%dT%H:%M","%Y-%m-%dT%H","%Y-%m-%d","%H:%M:%S","%H:%M","Nm","Nh","Nd"]

Some examples:
  aws-logs -g my-log-group -S -15m # start from 15 min ago

  aws-logs -g my-log-group -S 12:00 # start from noon local time

  aws-logs -g my-log-group -S 2222-01-02T03:04:05UTC # start from a UTC timestamp
```
