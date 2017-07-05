using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public struct Edge
{
    public int U { get; }
    public int V { get; }

    public Edge Dual()
    {
        return new Edge(V, U);
    }

    public Edge(int u, int v)
    {
        U = u;
        V = v;
    }
}

public static class ListExtensionForNextPermutation
{
    static void InplaceSwap<X>(IList<X> list, int i, int j)
    {
        var t = list[i];
        list[i] = list[j];
        list[j] = t;
    }

    static void InplaceReverse<X>(IList<X> list, int first, int count)
    {
        for (var i = 0; i < count / 2; i++)
        {
            InplaceSwap(list, first + i, first + count - 1 - i);
        }
    }

    public static bool NextPermutation<X>(this IList<X> list, IComparer<X> comparer)
    {
        var count = list.Count;
        if (count <= 1) return false;

        var i = count - 1;
        while (true)
        {
            var ii = i;

            i--;

            if (comparer.Compare(list[i], list[ii]) < 0)
            {
                var j = count - 1;
                while (comparer.Compare(list[i], list[j]) >= 0)
                {
                    j--;
                }

                InplaceSwap(list, i, j);
                InplaceReverse(list, ii, count - ii);
                return true;
            }

            if (i == 0)
            {
                InplaceReverse(list, 0, count);
                return false;
            }
        }
    }

    public static bool NextPermutation<X>(this IList<X> list)
    {
        return NextPermutation(list, Comparer<X>.Default);
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

    int n, m;
    Edge[] es;

    void Read()
    {
        {
            var line = ReadLine(int.Parse);
            n = line[0];
            m = line[1];
        }

        es = new Edge[m];
        for (var i = 0; i < m; i++)
        {
            var line = ReadLine(int.Parse);
            var u = line[0] - 1;
            var v = line[1] - 1;
            es[i] = new Edge(u, v);
        }
    }

    bool[,] Graph()
    {
        var g = new bool[n, n];

        foreach (var e in es)
        {
            g[e.U, e.V] = true;
            g[e.V, e.U] = true;
        }

        return g;
    }

    long Solve()
    {
        var p = Enumerable.Range(0, n - 1).ToArray();
        var g = Graph();

        var count = 0;

        do
        {
            // p: 始点以外の頂点からなる順列、とする。
            // 始点から始まり、この順番で辿る道があればOK。

            var ok = true;
            var v = 0;
            for (var i = 0; i < p.Length; i++)
            {
                var u = p[i] + 1;

                if (!g[v, u])
                {
                    ok = false;
                    break;
                }

                v = u;
            }

            if (ok)
            {
                count++;
            }
        }
        while (p.NextPermutation());
        return count;
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
