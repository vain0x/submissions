using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public static class TemplateExtension
{
    public static X[] MakeArray<X>(this int count, Func<int, X> func)
    {
        var xs = new X[count];
        for (var i = 0; i < count; i++)
        {
            xs[i] = func(i);
        }
        return xs;
    }

    public static int[] Range(this int count, int start = 0)
    {
        return count.MakeArray(i => i + start);
    }

    public static string Intercalate<X>(this IEnumerable<X> @this, string separator)
    {
        return string.Join(separator, @this);
    }

    public sealed class ValueIndexPair<T>
        : Tuple<T, int>
    {
        public T Value { get { return Item1; } }
        public int Index { get { return Item2; } }

        public ValueIndexPair(T value, int index)
            : base(value, index)
        {
        }
    }

    public static IEnumerable<ValueIndexPair<X>> Indexed<X>(this IEnumerable<X> @this)
    {
        var i = 0;
        foreach (var x in @this)
        {
            yield return new ValueIndexPair<X>(x, i);
            i++;
        }
    }
}

public sealed class Scanner
{
    private readonly TextReader _reader;
    private readonly StringBuilder _sb = new StringBuilder();

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        _sb.Clear();

        while (true)
        {
            var r = _reader.Read();

            if (r == '\r')
            {
                if (_reader.Peek() == '\n') _reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
            {
                break;
            }
            else
            {
                _sb.Append((char)r);
            }
        }

        return _sb.ToString();
    }

    /// <summary>
    /// Reads next word as <see cref="int"/>.
    /// </summary>
    public int N()
    {
        return int.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="long"/>.
    /// </summary>
    public long L()
    {
        return long.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="double"/>.
    /// </summary>
    public double F()
    {
        return double.Parse(Word());
    }

    public int[] Ns(int count)
    {
        return count.MakeArray(_ => N());
    }

    public long[] Ls(int count)
    {
        return count.MakeArray(_ => L());
    }

    public double[] Fs(int count)
    {
        return count.MakeArray(_ => F());
    }

    /// <summary>
    /// Reads next line and splits it by spaces.
    /// </summary>
    public X[] Words<X>(Func<string, X> func)
    {
        return _reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        _reader = reader;
    }
}

public partial class Program
{
    private readonly TextReader _input;
    private readonly TextWriter _output;
    private readonly Scanner _scanner;

    private void WriteLine(int value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(long value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(double value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(char value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(string value)
    {
        _output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        _input = input;
        _output = output;
        _scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).EntryPoint();
    }
}


public sealed class BinaryHeap<TValue>
    : IReadOnlyCollection<TValue>
{
    private readonly List<TValue> _list;
    private readonly Func<TValue, TValue, int> _compare;

    public int Count
    {
        get { return _list.Count; }
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return _list.GetEnumerator();
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public TValue Peek()
    {
        return _list[0];
    }

    public void Enqueue(TValue value)
    {
        _list.Add(value);
        var i = _list.Count - 1;
        while (i > 0)
        {
            // Index of the parent.
            var p = (i - 1) >> 1;

            if (_compare(_list[p], value) <= 0) break;
            _list[i] = _list[p];
            i = p;
        }
        _list[i] = value;
    }

    public TValue Dequeue()
    {
        var min = _list[0];
        var x = _list[_list.Count - 1];
        var i = 0;
        while (true)
        {
            // Index of children.
            var l = (i << 1) + 1;
            var r = (i << 1) + 2;
            if (l >= _list.Count) break;

            // Index of the smaller child.
            var c = r < _list.Count && _compare(_list[r], _list[l]) < 0 ? r : l;

            if (_compare(_list[c], x) >= 0) break;
            _list[i] = _list[c];
            i = c;
        }
        _list[i] = x;
        _list.RemoveAt(_list.Count - 1);
        return min;
    }

    public BinaryHeap(List<TValue> list, Func<TValue, TValue, int> compare)
    {
        _list = list;
        _compare = compare;
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

public sealed class Vertex
    : Tuple<int, int>
{
    public Vertex(int v, int cost)
        : base(cost, v)
    {
    }

    public int V
    {
        get { return Item2; }
    }

    public int Cost
    {
        get { return Item1; }
    }
}

public sealed partial class Program
{
    int h, w;
    int[][] cost;
    int[][] board;

    private long Solve()
    {
        var n = 10;

        // 点集合が [0, n] で、点 u から v へのコスト cost[v][u] の辺があるグラフを考える。
        // dist[u]: 1 から u への距離
        var dist = n.MakeArray(u => cost[u][1]);

        // Dijkstra
        var q = BinaryHeap.Create<Vertex>();
        for (var v = 0; v < n; v++)
        {
            q.Enqueue(new Vertex(v, dist[v]));
        }

        while (q.Count > 0)
        {
            var vertex = q.Dequeue();
            var u = vertex.V;

            for (var v = 0; v < n; v++)
            {
                var d = dist[u] + cost[v][u];
                if (dist[v] > d)
                {
                    dist[v] = d;
                    q.Enqueue(new Vertex(v, d));
                }
            }
        }

        return
            (
                from y in h.Range()
                from x in w.Range()
                let d = board[y][x]
                where d != -1
                select dist[d]
            ).Sum();
    }

    private void Read()
    {
        var a = _scanner;
        h = a.N();
        w = a.N();
        cost = 10.MakeArray(y => 10.MakeArray(x => a.N()));
        board = h.MakeArray(y => w.MakeArray(x => a.N()));
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
