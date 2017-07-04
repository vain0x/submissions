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

    int Dfs(bool[,] g, bool[] done, int u, int k)
    {
        if (k == n)
        {
            return 1;
        }
        else
        {
            var count = 0;

            for (var v = 1; v < n; v++)
            {
                if (!g[u, v] || done[v]) continue;

                done[v] = true;
                count += Dfs(g, done, v, k + 1);
                done[v] = false;
            }

            return count;
        }
    }

    int Solve()
    {
        var g = Graph();
        var done = new bool[n];
        return Dfs(g, done, 0, 1);
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
