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

public class PermutaionGenerator
{
    readonly int[][][] permutations;

    public int[][] PermutationGroup(int n)
    {
        return permutations[n];
    }

    public PermutaionGenerator(int n)
    {
        permutations = new int[n + 1][][];

        var count = 1;
        for (var r = 0; r <= n; r++)
        {
            permutations[r] = new int[count][];

            if (r == 0)
            {
                permutations[r][0] = new int[0];
            }
            else
            {
                var k = 0;

                foreach (var p in permutations[r - 1])
                {
                    for (var i = r - 1; i >= 0; i--)
                    {
                        var q = new int[r];
                        Array.Copy(p, q, i);
                        q[i] = r - 1;
                        Array.Copy(p, i, q, i + 1, r - (i + 1));

                        permutations[r][k] = q;
                        k++;
                    }
                }
            }

            count *= r + 1;
        }
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
        var ps = new PermutaionGenerator(n);
        var g = Graph();

        var count = 0;
        foreach (var p in ps.PermutationGroup(n - 1))
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
