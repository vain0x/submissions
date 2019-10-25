module Program

// -----------------------------------------------
// Binary trees
// -----------------------------------------------

[<RequireQualifiedAccess>]
type Tree<'a> =
  | Leaf
    of 'a
  | Node
    of Tree<'a> * Tree<'a>

let inline treeNewLeaf item =
  Tree.Leaf item

let inline treeNewNode (left: Tree<_>) (right: Tree<_>) =
  Tree.Node (left, right)

let treeGet w index (node: Tree<_>) =
  // assert (0 <= index && index < w)

  let rec go w index node =
    match node with
    | Tree.Leaf item ->
      // assert (w = 1 && index = 0)
      item

    | Tree.Node (left, right) ->
      let v = w / 2
      if index < v then
        go v index left
      else
        go v (index - v) right

  go w index node

let treeSet w index newItem (node: Tree<_>) =
  // assert (0 <= index && index < w)

  let rec go w index node =
    match node with
    | Tree.Leaf _ ->
      // assert (w = 1 && index = 0)
      treeNewLeaf newItem

    | Tree.Node (left, right) ->
      let v = w / 2
      if index < v then
        let left = go v index left
        treeNewNode left right
      else
        let right = go v (index - v) right
        treeNewNode left right

  go w index node

// -----------------------------------------------
// Zero-less binary digits
// -----------------------------------------------

// [<Struct>]
[<RequireQualifiedAccess>]
type Digit<'a> =
  | One
    of Tree<'a>
  | Two
    of Tree<'a> * Tree<'a>

let inline digitNewOne (node: Tree<_>) =
  Digit.One node

let inline digitNewTwo (left: Tree<_>) (right: Tree<_>) =
  Digit.Two (left, right)

// -----------------------------------------------
// Random access lists
// -----------------------------------------------

let ralNewEmpty defaultItem =
  0, [], defaultItem

let ralIsEmpty ral =
  let _, digits, _ = ral
  digits |> List.isEmpty

let ralToLength ral =
  let len, _, _ = ral
  len

let ralPush item ral =
  let len, digits, defaultItem = ral
  // assert ([item; defaultItem] |> List.isEmpty |> not)

  let rec go w t ds =
    match ds with
    | [] ->
      [digitNewOne t]

    | Digit.One u :: ds ->
      digitNewTwo t u :: ds

    | Digit.Two (left, right) :: ds ->
      let u = treeNewNode left right
      digitNewOne t :: go (w * 2) u ds

  let digits = go 1 (treeNewLeaf item) digits
  len + 1, digits, defaultItem

let ralGet index ral =
  let len, digits, defaultItem = ral
  // assert (0 <= index && index < len)

  let rec go w index digits =
    match digits with
    | [] ->
      // assert false
      defaultItem

    | d :: ds ->
      match d with
      | Digit.One node ->
        if index < w then
          node |> treeGet w index
        else
          go (w * 2) (index - w) ds

      | Digit.Two (left, right) ->
        if index < w then
          left |> treeGet w index
        else if index < w * 2 then
          right |> treeGet w (index - w)
        else
          go (w * 2) (index - w * 2) ds

  let index = len - index - 1
  go 1 index digits

let ralSet index newItem ral =
  let len, digits, defaultItem = ral
  // assert (0 <= index && index < len)
  // assert ([newItem; defaultItem] |> List.isEmpty |> not)

  let rec go w index digits =
    match digits with
    | [] ->
      // assert false
      digits

    | d :: ds ->
      match d with
      | Digit.One node ->
        if index < w then
          let t = node |> treeSet w index newItem
          digitNewOne t :: ds
        else
          d :: go (w * 2) (index - w) ds

      | Digit.Two (left, right) ->
        if index < w then
          let left = left |> treeSet w index newItem
          digitNewTwo left right :: ds
        else if index < w * 2 then
          let right = right |> treeSet w (index - w) newItem
          digitNewTwo left right :: ds
        else
          d :: go (w * 2) (index - w * 2) ds

  let index = len - index - 1
  let digits = go 1 index digits
  len, digits, defaultItem

let ralReplicate len item =
  // assert (len >= 0)

  if len = 0 then
    ralNewEmpty item
  else

  let treeReplicate w item =
    let rec go w =
      if w = 1 then
        treeNewLeaf item
      else
        let t = go (w / 2)
        treeNewNode t t
    go w

  let rec go w n =
    let t = treeReplicate w item

    if n = 0 then
      []
    else if n % (w * 2) = 0 then
      digitNewTwo t t :: go (w * 2) (n - w * 2)
    else
      // assert ((n - w) % (w * 2) = 0)
      digitNewOne t :: go (w * 2) (n - w)

  let digits = go 1 len
  len, digits, item

// -----------------------------------------------
// ABC 140 E
// -----------------------------------------------

let abc140eSolve n (perm: int[]) =
  let prev, next =
    let rec go i prev next =
      if i > n then
        prev, next
      else
        let prev = prev |> ralPush i
        let next = next |> ralPush i
        go (i + 1) prev next

    let prev = ralNewEmpty 0 |> ralPush 0 |> ralPush 0
    let next = ralNewEmpty 0
    let prev, next = go 1 prev next
    let next = next |> ralPush (n + 1) |> ralPush (n + 1)
    prev, next

  // inverse of perm
  let pos =
    let rec go i pos =
      if i = n then
        pos
      else
        pos |> ralSet (perm.[i] - 1) (i + 1) |> go (i + 1)

    let pos = ralReplicate n 0
    pos |> go 0

  let rec go sum prev next p =
    if p = n then
      sum
    else
      let i = pos |> ralGet p

      let x = prev |> ralGet i
      let w = prev |> ralGet x
      // assert (w <= x && x < i)

      let y = next |> ralGet i
      let z = next |> ralGet y
      // assert (i < y && y <= z)

      let count = (x - w) * (y - i) + (i - x) * (z - y)
      let sum = sum + int64 (count * (p + 1))

      let prev = prev |> ralSet y x
      let next = next |> ralSet x y
      go sum prev next (p + 1)

  go 0L prev next 0

[<EntryPoint>]
let main _ =
  let n = stdin.ReadLine() |> int
  let perm = stdin.ReadLine().Split(' ') |> Array.map int
  let m = abc140eSolve n perm
  printfn "%s" (string m)
  0
