-module(live_handler).

-callback created(file:filename()) -> ok.
-callback updated(file:filename()) -> ok.
-callback deleted(file:filename()) -> ok.
