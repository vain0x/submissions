using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
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

public struct Edge
{
    public int U { get; }
    public int V { get; }
    public long Cost { get; }

    public Edge(int u, int v, long cost)
    {
        U = u;
        V = v;
        Cost = cost;
    }
}

public sealed partial class Program
{
    static long[] Dijkstra(IReadOnlyList<IReadOnlyList<Edge>> graph, int v0)
    {
        var dist = graph.Count.MakeArray(_ => long.MaxValue);
        dist[v0] = 0;

        var heap = BinaryHeap.Create<int>((x, y) => Comparer<long>.Default.Compare(dist[x], dist[y]));
        heap.Enqueue(v0);

        var done = new bool[graph.Count];
        while (heap.Count > 0)
        {
            var u = heap.Dequeue();
            done[u] = true;

            var d = dist[u];
            foreach (var e in graph[u])
            {
                if (done[e.V]) continue;

                if (dist[e.V] == long.MaxValue || dist[e.V] > d + e.Cost)
                {
                    dist[e.V] = d + e.Cost;
                    heap.Enqueue(e.V);
                }
            }
        }

        return dist;
    }

    public void EntryPoint()
    {
        var s = scanner;
        var n = s.N();
        var g = n.MakeArray(_ => new List<Edge>());

        for (var i = 0; i < n - 1; i++)
        {
            var u = s.N() - 1;
            var v = s.N() - 1;
            var w = s.L();
            g[u].Add(new Edge(u, v, w));
            g[v].Add(new Edge(v, u, w));
        }

        var q = s.N();
        var k = s.N() - 1;

        var dist = Dijkstra(g, k);

        for (var i = 0; i < q; i++)
        {
            var u = s.N() - 1;
            var v = s.N() - 1;
            WriteLine(dist[u] + dist[v]);
        }
    }
}

#region Custom Library
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

    public static void ForEach<X>(this IEnumerable<X> @this, Action<X, int> action)
    {
        var list = @this as IReadOnlyList<X>;
        if (list != null)
        {
            var count = list.Count;
            for (var i = 0; i < count; i++)
            {
                action(list[i], i);
            }
        }
        else
        {
            var i = 0;
            foreach (var x in @this)
            {
                action(x, i);
                i++;
            }
        }
    }
}

public sealed class Scanner
{
    readonly TextReader reader;

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        var sb = default(StringBuilder);
        var firstChar = default(char);
        var count = 0;

        while (true)
        {
            var r = reader.Read();

            if (r == '\r')
            {
                if (reader.Peek() == '\n') reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
            {
                break;
            }
            else
            {
                var c = (char)r;

                switch (count)
                {
                    case 0:
                        firstChar = c;
                        count = 1;
                        break;
                    case 1:
                        sb = new StringBuilder();
                        sb.Append(firstChar).Append(c);
                        count = 2;
                        break;
                    default:
                        sb.Append(c);
                        break;
                }
            }
        }

        switch (count)
        {
            case 0:
                return "";
            case 1:
                return firstChar.ToString();
            default:
                return sb.ToString();
        }
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
    public double F
    {
        get
        {
            return double.Parse(Word());
        }
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
        return count.MakeArray(_ => F);
    }

    /// <summary>
    /// Reads next line and splits it by spaces.
    /// </summary>
    public X[] Words<X>(Func<string, X> func)
    {
        return reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        this.reader = reader;
    }
}

public partial class Program
{
    readonly TextReader input;
    readonly TextWriter output;
    readonly Scanner scanner;

    void WriteLine(int value)
    {
        output.WriteLine(value);
    }

    void WriteLine(long value)
    {
        output.WriteLine(value);
    }

    void WriteLine(double value)
    {
        output.WriteLine(value);
    }

    void WriteLine(char value)
    {
        output.WriteLine(value);
    }

    void WriteLine(string value)
    {
        output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        this.input = input;
        this.output = output;
        scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
#if DEBUG
        using (var writer = new VainZero.IO.DebugTextWriter(Console.Out))
#else
        var writer = Console.Out;
#endif
        {
            new Program(Console.In, writer).EntryPoint();
        }
    }
}
#endregion
