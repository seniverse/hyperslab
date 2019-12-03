-module(hyperslab_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_tiles, test_validate, test_subslabs, test_subsets, test_all].

test_tiles(_Config) ->
    [{[0,0],[{0,2},{4,2}]},
     {[0,2],[{2,2},{6,2}]},
     {[2,0],[{8,2},{12,2}]},
     {[2,2],[{10,2},{14,2}]}] = hyperslab:tiles([{4, 2}, {4, 2}]),

    [{[0,0],[{0,2},{3,2}]},
     {[0,2],[{2,1},{5,1}]},
     {[2,0],[{6,2}]},
     {[2,2],[{8,1}]}] = hyperslab:tiles([{3, 2}, {3, 2}]).

test_validate(_Config) ->
    true = hyperslab:validate([1], [{2, 2}]),
    false = hyperslab:validate([2], [{2, 2}]),
    true = hyperslab:validate([#{}], [{2, 2}]),
    true = hyperslab:validate([#{step=>5}], [{2, 2}]),
    true = hyperslab:validate([#{first=>1, step=>2}], [{2, 2}]),
    false = hyperslab:validate([#{first=>1, step=>2, last=>3}], [{2, 2}]),
    false = hyperslab:validate([#{first=>1, step=>2, last=>6}], [{5, 2}]).

test_subslabs(_Config) ->
    [{[0,0],[#{first := 0,last := 1},#{first := 0,last := 1}]},
     {[0,2],[#{first := 0,last := 1},#{first := 0,last := 1}]},
     {[2,0],[#{first := 0,last := 1},#{first := 0,last := 1}]},
     {[2,2],[#{first := 0,last := 1},#{first := 0,last := 1}]}] =
        hyperslab:subslabs(
          [#{},#{}],
          [{4,2}, {4,2}]),

    [{[0,0],[#{first := 0,last := 1},#{first := 0,last := 1}]},
     {[0,2],[#{first := 0,last := 1},0]},
     {[2,0],[0,#{first := 0,last := 1}]},
     {[2,2],[0,0]}] =
        hyperslab:subslabs(
          [#{},#{}],
          [{3,2}, {3,2}]),

    [{[0,0],[1,1]}] =
        hyperslab:subslabs(
          [1,1],
          [{4,2}, {4,2}]),

    [{[0,0],[0,0]},
     {[0,2],[0,0]},
     {[2,0],[0,0]},
     {[2,2],[0,0]}] =
        hyperslab:subslabs(
          [#{step => 2}, #{step => 2}],
          [{4,2}, {4,2}]),

    [{[0,0],
      [#{first := 0,last := 2,step := 2},
       #{first := 0,last := 2,step := 2}]},
     {[0,3],[#{first := 0,last := 2,step := 2},1]},
     {[3,0],[1,#{first := 0,last := 2,step := 2}]},
     {[3,3],[1,1]}] =
        hyperslab:subslabs(
          [#{step => 2}, #{step => 2}],
          [{5,3}, {5,3}]).

test_subsets(_Config) ->
    [{[0,0], {0,2}},
     {[0,2], {0,2}},
     {[0,0], {2,2}},
     {[0,2], {2,2}},
     {[2,0], {0,2}},
     {[2,2], {0,2}},
     {[2,0], {2,2}},
     {[2,2], {2,2}}] =
        hyperslab:subsets(
          [#{},#{}],
          [{4,2}, {4,2}]),

    [{[0,0], {0,2}},
     {[0,2], {0,1}},
     {[0,0], {2,2}},
     {[0,2], {1,1}},
     {[2,0], {0,2}},
     {[2,2], {0,1}}] =
        hyperslab:subsets(
          [#{},#{}],
          [{3,2}, {3,2}]),

    [{[0,0], {0,1}}] =
        hyperslab:subsets(
          [1,1],
          [{4,2}, {4,2}]),

    [{[0,0],{0,1}},
     {[0,2],{0,1}},
     {[2,0],{0,1}},
     {[2,2],{0,1}}] =
        hyperslab:subsets(
          [#{step => 2}, #{step => 2}],
          [{4,2}, {4,2}]),

    [{[0,0],{0,2}},
     {[0,3],{0,1}},
     {[0,0],{2,2}},
     {[0,3],{1,1}},
     {[3,0],{0,2}},
     {[3,3],{0,1}}] =
        hyperslab:subsets(
          [#{step => 2}, #{step => 2}],
          [{5,3}, {5,3}]).


make_data(Sizes) ->
    Size = lists:foldl(fun(S, Acc) -> S * Acc end, 1, Sizes),
    << <<(rand:uniform(256)-1)>> || _ <- lists:seq(1, Size) >>.

extract(Subsets, Tiles) ->
    << <<(binary:part(maps:get(Tile, Tiles), Part))/binary>>
       || {Tile, Part} <- Subsets>>.

extract(Data, Hyperslab, Dimensions) ->
    Tiles =
        maps:from_list(
          [{Tile, << <<(binary:part(Data, Part))/binary>> || Part <- Parts >>}
           || {Tile, Parts} <- hyperslab:tiles(Dimensions)]),

    Subslabs = hyperslab:subslabs(Hyperslab, Dimensions),

    Tiles1 =
        maps:from_list(
          [ {Tile,
             << <<(binary:part(maps:get(Tile, Tiles), Part))/binary>>
                || Part <- hyperslab:parts(Tile, Slab, Dimensions) >>}
            || {Tile, Slab} <- Subslabs ]),

    extract(hyperslab:subsets(Hyperslab, Dimensions), Tiles1).


test_tile(Hyperslab, Dimensions) ->
    {Sizes, _} = lists:unzip(Dimensions),
    Data = make_data(Sizes),
    Expected = extract(Data, Hyperslab, lists:zip(Sizes, Sizes)),
    Expected = extract(Data, Hyperslab, Dimensions),
    ok.

test_all(_Config) ->
    test_tile([5], [{10,3}]),
    test_tile([#{}, #{}], [{5,3},{5,4}]),
    test_tile([#{first=>1, step=>3, last=>17}, #{first=>3, step=>7, last=>18}], [{20,7},{20,3}]),
    test_tile([#{first=>1, step=>3, last=>17}, #{first=>3, step=>7, last=>18}, #{first=>1, step=>3, last=>17}, #{first=>3, step=>7, last=>18}], [{20,7},{20,3},{20,7},{20,3}]),
    ok.
