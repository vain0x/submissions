using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public static class EnumerableExtension
{
    public static IEnumerable<Y> Pairwise<X, Y>(this IEnumerable<X> @this, Func<X, X, Y> selector)
    {
        var previousX = default(X);
        var i = 0;
        foreach (var x in @this)
        {
            if (i > 0)
            {
                yield return selector(previousX, x);
            }

            previousX = x;
            i++;
        }
    }
}

public sealed class UnionFindForest
{
    readonly int[] parents;
    readonly int[] ranks;

    public UnionFindForest(int n)
    {
        parents = Enumerable.Range(0, n).ToArray();
        ranks = new int[n];
    }

    public int Root(int v)
    {
        if (parents[v] == v)
        {
            return v;
        }

        var root = Root(parents[v]);
        parents[v] = root;
        return root;
    }

    public bool Connects(int u, int v)
    {
        return Root(u) == Root(v);
    }

    static void Swap<X>(ref X l, ref X r)
    {
        var t = l;
        l = r;
        r = t;
    }

    public void Merge(int u, int v)
    {
        u = Root(u);
        v = Root(v);
        if (u == v) return;

        if (ranks[u] > ranks[v])
        {
            Swap(ref u, ref v);
        }

        parents[u] = v;
        ranks[v] += ranks[u];
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

    public long MinDistance(City second)
    {
        return Math.Min(Math.Abs(X - second.X), Math.Abs(Y - second.Y));
    }
}

public struct Pair<X, Y>
{
    public readonly X First;
    public readonly Y Second;

    public Pair(X first, Y second)
    {
        First = first;
        Second = second;
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

    Pair<City, City>[] OrderByDifference(IEnumerable<City> cities, Func<City, long> f)
    {
        return
            cities
            .OrderBy(f)
            .Pairwise((l, r) => new Pair<City, City>(l, r))
            .OrderBy(pair => f(pair.Second) - f(pair.First))
            .ToArray();
    }

    long Solve()
    {
        var dxs = OrderByDifference(cities, c => c.X);
        var dys = OrderByDifference(cities, c => c.Y);
        var uff = new UnionFindForest(n);

        var ix = 0;
        var iy = 0;
        var total = 0L;

        var mergeX =
            new Action(() =>
            {
                var p = dxs[ix];
                if (!uff.Connects(p.First.Id, p.Second.Id))
                {
                    uff.Merge(p.First.Id, p.Second.Id);
                    total += p.First.MinDistance(p.Second);
                }
                ix++;
            });
        var mergeY =
            new Action(() =>
            {
                var p = dys[iy];
                if (!uff.Connects(p.First.Id, p.Second.Id))
                {
                    uff.Merge(p.First.Id, p.Second.Id);
                    total += p.First.MinDistance(p.Second);
                }
                iy++;
            });

        while (true)
        {
            if (ix < dxs.Length && iy < dys.Length)
            {
                var px = dxs[ix];
                var dx = px.Second.X - px.First.X;

                var py = dys[iy];
                var dy = py.Second.Y - py.First.Y;

                if (dx <= dy)
                {
                    mergeX();
                }
                else
                {
                    mergeY();
                }
            }
            else if (ix < dxs.Length)
            {
                mergeX();
            }
            else if (iy < dys.Length)
            {
                mergeY();
            }
            else
            {
                break;
            }
        }

        return total;
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
