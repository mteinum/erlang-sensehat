%%% @doc Framebuffer module for Raspberry Pi Sense Hat
%%% Provides an 8x8 RGB pixel buffer with rotation support.
%%% Coordinates: Row (0-7), Col (0-7)
%%% @end

-module(shfb).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([create/1, set_pixel/4, set_pixels/2, to_binary/1, set_rotation/2]).

-type rgb() :: 0..16777215.
-type rotation() :: 0 | 90 | 180 | 270.
-type pixel_row() :: [rgb()].
-type pixel_matrix() :: [pixel_row()].

-record(framebuffer, {
    rotation :: rotation(),
    pixels :: pixel_matrix()
}).

-opaque framebuffer() :: #framebuffer{}.
-export_type([framebuffer/0]).

-spec setnth(pos_integer(), list(), term()) -> list().
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

-spec create_row(rgb()) -> pixel_row().
create_row(Color) ->
    [Color || _ <- lists:seq(1, 8)].

-spec create(rgb()) -> framebuffer().
create(Color) ->
    #framebuffer{
        rotation = 0,
        pixels = [create_row(Color) || _ <- lists:seq(1, 8)]
    }.

-spec set_rotation(rotation(), framebuffer()) -> framebuffer().
set_rotation(N, FB) when N =:= 0; N =:= 90; N =:= 180; N =:= 270 ->
    FB#framebuffer{rotation = N}.

-spec set_pixel(pos_integer(), pos_integer(), rgb(), framebuffer()) -> framebuffer().
set_pixel(Row, Col, Color, FB) when Row >= 1, Row =< 8, Col >= 1, Col =< 8 ->
    %% Extract the row
    RowToChange = lists:nth(Row, FB#framebuffer.pixels),

    %% Change the column in that row
    ChangedRow = setnth(Col, RowToChange, Color),

    %% Create a new matrix with the changed row
    FB#framebuffer{pixels = setnth(Row, FB#framebuffer.pixels, ChangedRow)}.

-spec set_pixels(pixel_matrix(), framebuffer()) -> framebuffer().
set_pixels(Pixels, FB) when length(Pixels) =:= 8 ->
    FB#framebuffer{pixels = Pixels}.

get_render_buffer(#framebuffer{pixels=Pixels, rotation=0}) ->
	Pixels;

get_render_buffer(#framebuffer{pixels=Pixels, rotation=90}) ->
	rotate90(Pixels);

get_render_buffer(#framebuffer{pixels=Pixels, rotation=180}) ->
	rotate90(rotate90(Pixels));

get_render_buffer(#framebuffer{pixels=Pixels, rotation=270}) ->
	transpose(Pixels).

-spec rotate90(pixel_matrix()) -> pixel_matrix().
rotate90(Pixels) ->
    reverse_rows(transpose(Pixels)).

-spec reverse_rows(pixel_matrix()) -> pixel_matrix().
reverse_rows(Pixels) ->
    lists:map(fun(E) -> lists:reverse(E) end, Pixels).

-spec to_binary(framebuffer()) -> binary().
to_binary(FB) ->
    list_to_binary([<<RGB:24>> || RGB <- lists:flatten(get_render_buffer(FB))]).

%% @doc Matrix transposition (from Haskell implementation)
%% Converts rows to columns
-spec transpose(pixel_matrix()) -> pixel_matrix().
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [].