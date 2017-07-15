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

    int n;
    long[] xs;

    void Read()
    {
        n = ReadLine(int.Parse)[0];
        xs = ReadLine(long.Parse);
    }

    long Solve()
    {
        var min = long.MaxValue;

        var sum = xs.Sum();

        var s = xs[0];
        for (var i = 1; i < n; i++)
        {
            min = Math.Min(min, Math.Abs(s - (sum - s)));

            s += xs[i];
        }

        return min;
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
