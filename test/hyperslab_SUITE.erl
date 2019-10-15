-module(hyperslab_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_tiles, test_tiles2, test_subsets, test_validate, test_all].

test_tiles(_Config) ->
    [{[0,0],[{0,2},{4,2}]},
     {[0,2],[{2,2},{6,2}]},
     {[2,0],[{8,2},{12,2}]},
     {[2,2],[{10,2},{14,2}]}] = hyperslab:tiles([{4, 2}, {4, 2}]),

    [{[0,0],[{0,2},{3,2}]},
     {[0,2],[{2,1},{5,1}]},
     {[2,0],[{6,2}]},
     {[2,2],[{8,1}]}] = hyperslab:tiles([{3, 2}, {3, 2}]).

test_tiles2(_Config) ->
    [[0,0],[0,2],[2,0],[2,2]] =
        hyperslab:tiles(
          [#{},#{}],
          [{4,2}, {4,2}]).

test_subsets(_Config) ->
    [{[0,0], [{0,2}]},
     {[0,2], [{0,2}]},
     {[0,0], [{2,2}]},
     {[0,2], [{2,2}]},
     {[2,0], [{0,2}]},
     {[2,2], [{0,2}]},
     {[2,0], [{2,2}]},
     {[2,2], [{2,2}]}] =
        hyperslab:subsets(
          [#{},#{}],
          [{4,2}, {4,2}]),

    [{[0,0], [{0,2}]},
     {[0,2], [{0,1}]},
     {[0,0], [{2,2}]},
     {[0,2], [{1,1}]},
     {[2,0], [{0,2}]},
     {[2,2], [{0,1}]}] =
        hyperslab:subsets(
          [#{},#{}],
          [{3,2}, {3,2}]),

    [{[0,0], [{3,1}]}] =
        hyperslab:subsets(
          [1,1],
          [{4,2}, {4,2}]),

    [{[0,0],[{0,1}]},
     {[0,2],[{0,1}]},
     {[2,0],[{0,1}]},
     {[2,2],[{0,1}]}] =
        hyperslab:subsets(
          [#{step => 2}, #{step => 2}],
          [{4,2}, {4,2}]),

    [{[0,0],[{0,1},{2,1}]},
     {[0,3],[{1,1}]},
     {[0,0],[{6,1},{8,1}]},
     {[0,3],[{5,1}]},
     {[3,0],[{3,1},{5,1}]},
     {[3,3],[{3,1}]}] =
        hyperslab:subsets(
          [#{step => 2}, #{step => 2}],
          [{5,3}, {5,3}]).


test_validate(_Config) ->
    true = hyperslab:validate([1], [{2, 2}]),
    false = hyperslab:validate([2], [{2, 2}]),
    true = hyperslab:validate([#{}], [{2, 2}]),
    true = hyperslab:validate([#{step=>5}], [{2, 2}]),
    true = hyperslab:validate([#{first=>1, step=>2}], [{2, 2}]),
    false = hyperslab:validate([#{first=>1, step=>2, last=>3}], [{2, 2}]),
    false = hyperslab:validate([#{first=>1, step=>2, last=>6}], [{5, 2}]).


make_data(Dimensions) ->
    Size = lists:foldl(fun({S, _}, Acc) -> S * Acc end, 1, Dimensions),
    << <<(rand:uniform(256)-1)>> || _ <- lists:seq(1, Size) >>.

extract(Subsets, Tiles) ->
    << <<(binary:part(maps:get(Tile, Tiles), Part))/binary>>
       || {Tile, Parts} <- Subsets,
          Part <- Parts >>.

test_tile(Hyperslab, Dimensions) ->
    Data = make_data(Dimensions),
    Tiles = maps:from_list(
              [{Tile, << <<(binary:part(Data, Part))/binary>> || Part <- Parts >>}
               || {Tile, Parts} <- hyperslab:tiles(Dimensions)]),
    Expected =
        extract(
          hyperslab:subsets(Hyperslab, [{Size, Size} || {Size, _} <- Dimensions ]),
          #{[0 || _ <- Dimensions] => Data}),
    Expected = extract(hyperslab:subsets(Hyperslab, Dimensions), Tiles).

test_all(_Config) ->
    test_tile([5], [{10,3}]),
    test_tile([#{}, #{}], [{5,3},{5,4}]),
    test_tile([#{first=>1, step=>3, last=>17}, #{first=>3, step=>7, last=>18}], [{20,7},{20,3}]).
