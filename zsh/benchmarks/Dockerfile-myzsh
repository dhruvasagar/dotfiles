# vim: ft=Dockerfile

FROM alpine

LABEL maintainer="Dhruva Sagar <dhruva@tarkalabs.com>"

RUN apk update --no-cache && apk add --no-cache zsh

RUN sed -i -e "s/bin\/ash/bin\/zsh/" /etc/passwd

RUN mkdir -p /root/dotfiles/zsh
COPY . /root/dotfiles/zsh/

RUN ln -s /root/dotfiles/zsh/zshrc /root/.zshrc

COPY benchmarks/timezsh /

CMD "./timezsh"
