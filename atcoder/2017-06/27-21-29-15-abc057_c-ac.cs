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

    long n;

    int Length(long x)
    {
        if (x == 0) return 1;

        var length = 0;
        while (true)
        {
            if (x == 0) return length;

            x = x / 10;
            length++;
        }
    }

    int F(long x, long y)
    {
        return Math.Max(Length(x), Length(y));
    }

    int Solve()
    {
        var m = int.MaxValue;

        for (var x = 1L; x * x <= n; x++)
        {
            if (n % x != 0) continue;
            var y = n / x;
            m = Math.Min(m, F(x, y));
        }

        return m;
    }

    public void Run()
    {
        n = ReadLine(long.Parse)[0];
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
