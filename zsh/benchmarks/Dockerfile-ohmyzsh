# vim: ft=Dockerfile

FROM alpine

LABEL maintainer="Dhruva Sagar <dhruva@tarkalabs.com>"

RUN apk update --no-cache && apk add --no-cache zsh git

RUN sed -i -e "s/bin\/ash/bin\/zsh/" /etc/passwd

COPY benchmarks .

RUN ./install_ohmyzsh

RUN rm -f /root/.zshrc
RUN ln -s /zshrc /root/.zshrc

CMD "./timezsh"
