using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class BinaryHeap<TValue>
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
}

public static class EnumerableExtension
{
    public static IEnumerable<Y> Pairwise<X, Y>(this IEnumerable<X> @this, Func<X, X, Y> selector)
    {
        using (var enumerator = @this.GetEnumerator())
        {
            if (!enumerator.MoveNext()) yield break;
            var previousX = enumerator.Current;

            while (enumerator.MoveNext())
            {
                var x = enumerator.Current;
                yield return selector(previousX, x);
                previousX = x;
            }
        }
    }
}

public struct City
{
    public readonly int Id;
    public readonly long X;
    public readonly long Y;

    public City(int id, long x, long y)
    {
        Id = id;
        X = x;
        Y = y;
    }

    public long Cost(City second)
    {
        return Math.Min(Math.Abs(X - second.X), Math.Abs(Y - second.Y));
    }
}

public struct Edge
{
    public readonly City First;
    public readonly City Second;
    public readonly long Cost;

    public Edge(City first, City second, long cost)
    {
        First = first;
        Second = second;
        Cost = cost;
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
    City[] cities;

    void Read()
    {
        n = ReadLine(int.Parse)[0];
        cities =
            (
                from i in Enumerable.Range(0, n)
                let line = ReadLine(long.Parse)
                select new City(i, line[0], line[1])
            ).ToArray();
    }

    IEnumerable<Edge> Edges(IEnumerable<City> cities, Func<City, long> f)
    {
        return cities.OrderBy(f).Pairwise((l, r) => new Edge(l, r, f(r) - f(l)));
    }

    long Solve()
    {
        var edges = Edges(cities, c => c.X).Concat(Edges(cities, c => c.Y));

        var graph = Enumerable.Range(0, n).Select(_ => new List<Edge>()).ToArray();
        foreach (var edge in edges)
        {
            graph[edge.First.Id].Add(edge);
            graph[edge.Second.Id].Add(edge);
        }

        var heap = BinaryHeap.Create<Edge>((l, r) => Comparer<long>.Default.Compare(l.Cost, r.Cost));
        var set = new HashSet<int>();
        var cost = 0L;

        foreach (var e in graph[0])
        {
            heap.Enqueue(e);
        }

        while (set.Count < n)
        {
            var edge = heap.Dequeue();
            if (set.Contains(edge.First.Id) && set.Contains(edge.Second.Id)) continue;

            if (set.Add(edge.First.Id))
            {
                foreach (var e in graph[edge.First.Id])
                {
                    heap.Enqueue(e);
                }
            }

            if (set.Add(edge.Second.Id))
            {
                foreach (var e in graph[edge.Second.Id])
                {
                    heap.Enqueue(e);
                }
            }

            cost += edge.Cost;
        }

        return cost;
    }

    public void Run()
    {
        Read();
        WriteLineOne(Solve());
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
