using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
    int[] xs;
    int[] ys;

    void Read()
    {
        {
            var l = ReadLine(int.Parse);
            n = l[0];
            m = l[1];
        }

        xs = new int[m];
        ys = new int[m];

        for (var i = 0; i < m; i++)
        {
            var l = ReadLine(int.Parse);
            xs[i] = l[0] - 1;
            ys[i] = l[1] - 1;
        }
    }

    bool Solve()
    {
        var middles = new HashSet<int>();
        for (var i = 0; i < m; i++)
        {
            if (ys[i] == n - 1)
            {
                middles.Add(xs[i]);
            }
        }

        for (var i = 0; i < m; i++)
        {
            if (xs[i] == 0)
            {
                if (middles.Contains(ys[i])) return true;
            }
        }

        return false;
    }

    public void Run()
    {
        Read();
        WriteLineOne(Solve() ? "POSSIBLE" : "IMPOSSIBLE");
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
