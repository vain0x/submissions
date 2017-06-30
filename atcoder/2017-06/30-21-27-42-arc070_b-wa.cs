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

    int n, k;
    long[] xs;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        k = line[1];
        xs = ReadLine(long.Parse);
    }

    int Solve()
    {
        Array.Sort(xs);

        var count = 0;

        for (var j = 0; j < xs.Length; j++)
        {
            var sum = xs[j];

            for (var i = 0; i < xs.Length; i++)
            {
                if (i == j) continue;
                if (sum >= k) break;
                sum += xs[i];
            }

            if (sum < k || sum - xs[j] >= k)
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
