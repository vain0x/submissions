using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class BinaryHeap<TValue>
    : IReadOnlyCollection<TValue>
{
    readonly List<TValue> list;
    readonly Func<TValue, TValue, int> compare;

    public int Count
    {
        get { return list.Count; }
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return list.GetEnumerator();
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public TValue Peek()
    {
        return list[0];
    }

    public void Enqueue(TValue value)
    {
        list.Add(value);
        var i = list.Count - 1;
        while (i > 0)
        {
            // Index of the parent.
            var p = (i - 1) >> 1;

            if (compare(list[p], value) <= 0) break;
            list[i] = list[p];
            i = p;
        }
        list[i] = value;
    }

    public TValue Dequeue()
    {
        var min = list[0];
        var x = list[list.Count - 1];
        var i = 0;
        while (true)
        {
            // Index of children.
            var l = (i << 1) + 1;
            var r = (i << 1) + 2;
            if (l >= list.Count) break;

            // Index of the smaller child.
            var c = r < list.Count && compare(list[r], list[l]) < 0 ? r : l;

            if (compare(list[c], x) >= 0) break;
            list[i] = list[c];
            i = c;
        }
        list[i] = x;
        list.RemoveAt(list.Count - 1);
        return min;
    }

    public BinaryHeap(List<TValue> list, Func<TValue, TValue, int> compare)
    {
        this.list = list;
        this.compare = compare;
    }
}

public static class BinaryHeap
{
    public static BinaryHeap<X> Create<X>(Func<X, X, int> compare)
    {
        return new BinaryHeap<X>(new List<X>(), compare);
    }

    public static BinaryHeap<X> Create<X>()
    {
        return new BinaryHeap<X>(new List<X>(), Comparer<X>.Default.Compare);
    }

    public static BinaryHeap<X> FromEnumerable<X>(IEnumerable<X> xs, Func<X, X, int> compare)
    {
        var list = new List<X>(xs);
        list.Sort(new Comparison<X>(compare));
        return new BinaryHeap<X>(list, compare);
    }

    public static BinaryHeap<X> FromEnumerable<X>(IEnumerable<X> xs)
    {
        return FromEnumerable(xs, Comparer<X>.Default.Compare);
    }
}

sealed class Cell
    : Tuple<int, int>
{
    public int Value
    {
        get { return Item1; }
    }

    public int Index
    {
        get { return Item2; }
    }

    public Cell(int value, int index)
        : base(value, index)
    {
    }

    public static readonly Cell Max = new Cell(int.MaxValue, -1);
}

sealed class Block
{
    public BinaryHeap<Cell> EvenCells;
    public BinaryHeap<Cell> OddCells;

    public Block(IReadOnlyList<Cell> cells)
    {
        var evens = new Cell[cells.Count / 2];
        var odds = new Cell[cells.Count / 2];

        for (var i = 0; i < cells.Count; i++)
        {
            if (i % 2 == 0)
            {
                evens[i / 2] = cells[i];
            }
            else
            {
                odds[i / 2] = cells[i];
            }
        }

        EvenCells = BinaryHeap.FromEnumerable(evens);
        OddCells = BinaryHeap.FromEnumerable(odds);
    }

    public void Remove(Cell cell)
    {
        var es = new List<Cell>();
        var os = new List<Cell>();

        foreach (var c in EvenCells)
        {
            if (c.Index < cell.Index)
            {
                es.Add(c);
            }
            else if (c.Index > cell.Index)
            {
                os.Add(c);
            }
        }

        foreach (var c in OddCells)
        {
            if (c.Index < cell.Index)
            {
                os.Add(c);
            }
            else if (c.Index > cell.Index)
            {
                es.Add(c);
            }
        }

        EvenCells = BinaryHeap.FromEnumerable(es);
        OddCells = BinaryHeap.FromEnumerable(os);
    }

    public void Switch()
    {
        var t = EvenCells;
        EvenCells = OddCells;
        OddCells = t;
    }
}

public class Program
{
    #region Standard I/O
    readonly TextReader input;
    readonly TextWriter output;

    X[] ReadLine<X>(Func<string, X> func)
    {
        return input.ReadLine().Split(' ').Select(func).ToArray();
    }

    void WriteLineOne(object obj)
    {
        output.WriteLine("{0}", obj);
    }

    void WriteLineMany(params object[] objects)
    {
        output.WriteLine(string.Join(" ", objects.Select(obj => obj.ToString())));
    }
    #endregion

    int n;
    int[] ps;

    IEnumerable<int> Solve()
    {
        {
            n = ReadLine(int.Parse)[0];
            ps = ReadLine(int.Parse);
        }

        var m = Math.Max(((int)Math.Sqrt(n) / 2) * 2, 32);

        var qs = new List<int>();

        var allCells = ps.Select((p, i) => new Cell(p, i)).ToArray();
        var blocks =
            Enumerable.Range(0, (n + m - 1) / m)
            .Select(i => new Block(new ArraySegment<Cell>(allCells, i * m, Math.Min(m, n - i * m))))
            .ToArray();

        for (var r = 0; r < n / 2; r++)
        {
            {
                var minX = Cell.Max;
                var minXBlockIndex = -1;

                for (var i = 0; i < blocks.Length; i++)
                {
                    var cells = blocks[i].EvenCells;

                    if (cells.Count == 0) continue;
                    var x = cells.Peek();
                    if (minX.Value > x.Value)
                    {
                        minX = x;
                        minXBlockIndex = i;
                    }
                }

                var minY = Cell.Max;
                var minYBlockIndex = -1;

                // x より後ろの最小の y を探索する。
                for (var i = minXBlockIndex; i < blocks.Length; i++)
                {
                    var block = blocks[i];
                    if (block.OddCells.Count == 0) continue;

                    var y = default(Cell);

                    if (i == minXBlockIndex)
                    {
                        // x が選出されたブロックではキューが使えないので全探索する。
                        y = block.OddCells.Where(c => c.Index > minX.Index).Min();
                    }
                    else
                    {
                        y = block.OddCells.Peek();
                    }

                    if (minY.Value > y.Value)
                    {
                        minY = y;
                        minYBlockIndex = i;
                    }
                }

                // minX, minY を p から q に移動する。
                qs.Add(minX.Value);
                qs.Add(minY.Value);

                // インデックスを更新する。
                {
                    // x を取り除き、これより後ろのセルの偶奇が反転する。
                    for (var j = minXBlockIndex; j < blocks.Length; j++)
                    {
                        if (j == minXBlockIndex)
                        {
                            blocks[j].Remove(minX);
                            continue;
                        }

                        blocks[j].Switch();
                    }

                    // y を取り除き、これより後ろのセルの偶奇が反転する。
                    // (結局 y より後ろのブロックの偶奇を変更する必要はないが、どうせ計算量は変わらない。)
                    for (var j = minYBlockIndex; j < blocks.Length; j++)
                    {
                        if (j == minYBlockIndex)
                        {
                            blocks[j].Remove(minY);
                            continue;
                        }

                        blocks[j].Switch();
                    }
                }
            }
        }

        return qs;
    }

    public void Run()
    {
        WriteLineOne(string.Join(" ", Solve()));
    }

    public Program(TextReader input, TextWriter output)
    {
        this.input = input;
        this.output = output;
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).Run();
    }
}
