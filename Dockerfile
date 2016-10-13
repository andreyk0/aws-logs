FROM fpco/haskell-scratch:integer-gmp

ADD rootfs.tar /
ADD .stack-work/install/x86_64-linux/lts-7.3/8.0.1/bin/aws-logs /aws-logs

CMD [ "/aws-logs", "--help" ]
