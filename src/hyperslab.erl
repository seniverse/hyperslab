-module(hyperslab).

-export([tiles/1, validate/2, subslabs/2, parts/3, subsets/2]).

-type dimension() :: {pos_integer(), pos_integer()}.

-type tile() :: [non_neg_integer()].

-type part() :: {non_neg_integer(), pos_integer()}.

-type index() :: non_neg_integer()
               | #{first => non_neg_integer(),
                   step => pos_integer(),
                   last => non_neg_integer()}.

-type hyperslab() :: [index()].


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


-spec validate(hyperslab(), [dimension()]) -> boolean().

validate([], []) ->
    true;
validate([N|T1], [{Size, _}|T2]) when is_integer(N) ->
    (0 =< N) and (N < Size) and validate(T1, T2);
validate([Index|T1], [{Size, _}|T2]) when is_map(Index) ->
    {First, Last, Step} = normalize_index(Index, Size),
    Max = First + (Last - First) div Step * Step,
    (0 =< First) and (First =< Max) and (Max < Size) and validate(T1, T2).

normalize_index(Index, _Size) when is_integer(Index) ->
    {Index, Index, 1};
normalize_index(Index, Size) when is_map(Index) ->
    First = maps:get(first, Index, 0),
    Last = maps:get(last, Index, Size-1),
    Step = maps:get(step, Index, 1),
    {First, Last, Step}.


-spec subslabs(hyperslab(), [dimension()]) -> [{tile(), hyperslab()}].
subslabs(Hyperslab, Dimensions) ->
    subslabs(Hyperslab, Dimensions, fun slab/3).

-spec parts(tile(), hyperslab(), [dimension()]) -> [part()].
parts(Tile, Hyperslab, Dimensions) ->
    {_, Parts} = slices(Tile, Hyperslab, Dimensions),
    Parts.

slices([H1], [H2], [H3]) ->
    Size = tile_size(H1, H3),
    case normalize_index(H2, Size) of
        {First, Last, 1} ->
            {Size, [{First, Last - First + 1}]};
        {First, Last, Step} ->
            {Size, [{I, 1} || I <- lists:seq(First, Last, Step)]}
    end;
slices([H1|T1], [H2|T2], [H3|T3]) ->
    {Acc, Parts} = slices(T1, T2, T3),
    Size = tile_size(H1, H3),
    {First, Last, Step} = normalize_index(H2, Size),
    {Acc * Size,
     [{I * Acc + Offset, Length}
      || I <- lists:seq(First, Last, Step),
         {Offset, Length} <- Parts]}.

-spec subsets(hyperslab(), [dimension()]) -> [{tile(), part()}].

subsets(Hyperslab, Dimensions) ->
    [{Tile, Part} || {Tile, _, Part} <- slices(Hyperslab, Dimensions)].

slices([Index], [{Size, Tile}]) ->
    Subslabs = subslabs(Index, Size, Tile, fun slab_size/3),
    [{[N], S, {0, S}} || {N, S} <- Subslabs ];
slices([Index|T1], [{Size, Tile}|T2]) ->
    Slices = slices(T1, T2),
    Subslabs = subslabs(Index, Size, Tile, fun slab_size/3),
    [{[H|T], S * A, {I * A + Offset, Length}}
     || {H, S} <- Subslabs,
        I <- lists:seq(0, S-1),
        {T, A, {Offset, Length}} <- Slices].


subslabs([], [], _) ->
    [{[], []}];
subslabs([Index|T1], [{Size, Tile}|T2], Fun) ->
    H = subslabs(Index, Size, Tile, Fun),
    T = subslabs(T1, T2, Fun),
    [ {[H3|T3], [H4|T4]}
      || {H3,H4} <- H,
         {T3,T4} <- T ].

subslabs(Index, Size, Tile, Fun) ->
    {First, Last, Step} = normalize_index(Index, Size),
    subslabs(First, First + (Last - First) div Step * Step, Step, Tile, Fun).

subslabs(First, Last, Step, Tile, Fun) when First - First rem Tile < Last - Last rem Tile ->
    Offset = First rem Tile,
    Base = First - Offset,
    Max = Offset + (Tile - 1 - Offset) div Step * Step,
    [{Base, Fun(Offset, Max, Step)}|subslabs(Base + Max + Step, Last, Step, Tile, Fun)];
subslabs(First, Last, Step, Tile, Fun) when First =< Last ->
    Offset = First rem Tile,
    Base = First - Offset,
    [{Base, Fun(Offset, Last rem Tile, Step)}];
subslabs(_First, _Last, _Step, _Tile, _Fun) ->
    [].

slab(First, First, _Step) ->
    First;
slab(First, Last, 1) ->
    #{first => First, last => Last};
slab(First, Last, Step) ->
    #{first => First, last => Last, step => Step}.

slab_size(First, Last, Step) ->
    (Last - First) div Step + 1.
