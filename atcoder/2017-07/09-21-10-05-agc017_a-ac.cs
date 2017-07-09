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

    int n, p;
    int[] xs;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        p = line[1];

        xs = ReadLine(int.Parse);
    }

    long Solve()
    {
        var dp = new long[2, n + 1];
        dp[0, 0] = 1;

        for (var i = 0; i < n; i++)
        {
            for (var p = 0; p < 2; p++)
            {
                var q = p == (xs[i] % 2) ? 0 : 1;
                dp[p, i + 1] = dp[p, i] + dp[q, i];
            }
        }

        return dp[p, n];
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
