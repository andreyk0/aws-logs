FROM fpco/haskell-scratch:integer-gmp

ADD .stack-work/install/x86_64-linux/lts-4.1/7.10.3/bin/aws-logs /aws-logs

CMD [ "/aws-logs", "--help" ]
