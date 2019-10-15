-module(hyperslab).

-export([tiles/1, tiles/2, subsets/2, validate/2]).

-type dimension() :: {pos_integer(), pos_integer()}.

-type tile() :: [non_neg_integer()].

-type part() :: {non_neg_integer(), pos_integer()}.

-spec tiles([dimension()]) -> [{tile(), [part()]}].

tiles(Dimensions) ->
    Sizes = sizes(Dimensions),
    [{Tile, tiles(Tile, Dimensions, Sizes)} || Tile <- bases(Dimensions) ].

sizes(Dimensions) ->
    {Sizes, _} =
        lists:mapfoldr(
          fun (Size, Acc) ->
                  {Acc, Size * Acc}
          end,
          1,
          [Size || {Size, _} <- Dimensions]),
    Sizes.

bases([]) ->
    [[]];
bases([{Size, Tile}|T]) ->
    T1 = bases(T),
    [[X|Y]
     || X <- lists:seq(0, Size-1, Tile),
        Y <- T1].

tile_size(N, {Size, Tile}) when N div Tile == Size div Tile ->
    Size rem Tile;
tile_size(_, {_, Tile}) ->
    Tile.

tiles([H1], [H2], [1]) ->
    [{H1, tile_size(H1, H2)}];
tiles([H1|T1], [H2|T2], [H3|T3]) ->
    Parts = tiles(T1, T2, T3),
    Base = H1 * H3,
    [ Part
      || I <- lists:seq(0, tile_size(H1, H2)-1),
         Part <- parts(Base + I * H3, Parts) ].

parts(Base, Parts) ->
    [{Base + Offset, Size} || {Offset, Size} <- Parts].


-spec validate([index()], [dimension()]) -> boolean().

validate([], []) ->
    true;
validate([N|T1], [{Size, _}|T2]) when is_integer(N) ->
    (0 =< N) and (N < Size) and validate(T1, T2);
validate([Index|T1], [{Size, _}|T2]) when is_map(Index) ->
    First = maps:get(first, Index, 0),
    Last = maps:get(last, Index, Size-1),
    Step = maps:get(step, Index, 1),
    Max = First + (Last - First) div Step * Step,
    (0 =< First) and (First < Size) and (0 =< Max) and (Max < Size) and validate(T1, T2).


-type index() :: non_neg_integer()
               | #{first => non_neg_integer(),
                   step => pos_integer(),
                   last => non_neg_integer()}.

-spec tiles([index()], [dimension()]) -> [tile()].
tiles([H1], [H2]) ->
    [[N] || {N, _} <- slice(H1,H2)];
tiles([H1|T1], [H2|T2]) ->
    [[H|T]
     || {H, _} <- slice(H1, H2),
        T <- tiles(T1, T2)].

-spec subsets([index()], [dimension()]) -> [{tile(), [part()]}].

subsets(Hyperslab, Dimensions) ->
    [{Tile, Parts} || {Tile, _, Parts} <- slices(Hyperslab, Dimensions)].

slices([H1], [H2]) ->
    [{[N], tile_size(N, H2), Parts} || {N, Parts} <- slice(H1,H2)];
slices([H1|T1], [H2|T2]) ->
    Slices = slices(T1, T2),
    [Slice
     || {H, Parts} <- slice(H1,H2),
        {Offset, Length} <- Parts,
        I <- lists:seq(Offset, Offset+Length-1),
        Slice <- slices(H, I, tile_size(H, H2), Slices)].

slices(H, I, Tile, Slices) ->
    [ {[H|T], Tile * Size, parts(I * Size, Parts)}
      || {T, Size, Parts} <- Slices].


slice(N, {_, Tile}) when is_integer(N) ->
    [{N - N rem Tile, [{N rem Tile, 1}]}];
slice(Index, {Size, Tile}) ->
    First = maps:get(first, Index, 0),
    Last = maps:get(last, Index, Size-1),
    Step = maps:get(step, Index, 1),
    slice(First, Last, Step, Tile).

slice(First, Last, 1, Tile) when First - First rem Tile < Last - Last rem Tile ->
    Offset = First rem Tile,
    Base = First - Offset,
    [{Base, [{Offset, Tile - Offset}]}|slice(Base + Tile, Last, 1, Tile)];
slice(First, Last, 1, Tile) when First =< Last ->
    Offset = First rem Tile,
    [{First - Offset, [{Offset, Last + 1 - First}]}];
slice(First, Last, Step, Tile) ->
    merge([{N - N rem Tile, {N rem Tile, 1}} || N <- lists:seq(First, Last, Step)]).

merge([]) ->
    [];
merge([{Tile, Offset}|T]) ->
    case merge(T) of
        [{Tile, Offsets}|Rest] ->
            [{Tile, [Offset|Offsets]}|Rest];
        Rest ->
            [{Tile, [Offset]}|Rest]
    end.
