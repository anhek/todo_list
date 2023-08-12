FROM erlang:25-alpine

RUN apk add --no-cache git

RUN mkdir "/app"
WORKDIR /app
COPY ./ /app

RUN rebar3 release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache libgcc

COPY --from=0 /app/_build/default/rel/todo_list /todo_list

CMD ["/todo_list/bin/todo_list", "foreground"]
