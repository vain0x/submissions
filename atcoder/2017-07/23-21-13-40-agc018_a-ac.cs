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

    static long Gcd(long x, long y)
    {
        return y == 0 ? x : Gcd(y, x % y);
    }

    static long Gcd(long[] xs)
    {
        var x = xs[0];

        for (var i = 1; i < xs.Length; i++)
        {
            x = Gcd(x, xs[i]);
        }

        return x;
    }

    long k;
    long[] xs;

    void Read()
    {
        k = ReadLine(long.Parse)[1];
        xs = ReadLine(long.Parse);
    }

    bool Solve()
    {
        var max = xs.Max();
        if (k > max) return false;

        var g = Gcd(xs);
        return k % g == 0;
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
