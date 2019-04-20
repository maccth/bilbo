# Plan

## Parsing
- [x] parsing `&v` for getting node id and `&&v` for getting node load
- [x] parsing path comprehensions
- [x] parsing nested paths: `[a, >, [b,c,d], >, e]`
- [x] parsing transforms
- [ ] parsing basic pipelines
- [ ] parsing pipeline comprehensions

## Evaluating Analysis
- [x] constructing the variable symbol table
- [ ] constructing the transform symbol table
- [ ] LHS checking
- [ ] variable checking


Some notes...
```
T = T1 |> T2 |> T3 <| T4 |> T5

a = {T1, T2, T3 <| if a>10 |> T4, T5}

a = G |min|> filter


if a>10 then (
    lqhdo
    iqshdohasd
    aksjdajksd;)



if
| g |?> T1 -> g |>= T1 
| g |?> T2 -> g |>= T2
| g |?> T3 -> g |>= T3
;

if a>10:
    b = 30
else:
    b = 20


if
| a>20 and b>30 and c>40
    ->
    jdosdh
|



||min>

bestG = ||min> T\


if doAnother:
    g |>= T

if doAnother ->
    g |>= T

if
| doAnother ->
    g |>= T
| (not doAnother) and (doTwo) ->
    g |>= T2
| _ ->
    g |>= T3
;

g' = g |> T!

g = g |> t1 <|> t2

g = g |> if (a>10) then g3 else g4 |>
g = g |> if (a>10) -> g3 -/> g4 |>

```