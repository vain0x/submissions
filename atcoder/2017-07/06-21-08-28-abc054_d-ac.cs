using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public struct Medicine
{
    public int X;
    public int Y;
    public int Cost;
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

    int n, mx, my;
    Medicine[] medicines;
    int totalX, totalY;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        mx = line[1];
        my = line[2];

        medicines = new Medicine[n];
        for (var i = 0; i < n; i++)
        {
            var l = ReadLine(int.Parse);
            var x = l[0];
            var y = l[1];
            var c = l[2];

            medicines[i] =
                new Medicine()
                {
                    X = my * x,
                    Y = mx * y,
                    Cost = c,
                };

            totalX += medicines[i].X;
            totalY += medicines[i].Y;
        }
    }

    int Solve()
    {
        var dp = new int[totalX + 1, totalY + 1];

        for (var x = 0; x <= totalX; x++)
        {
            for (var y = 0; y <= totalY; y++)
            {
                dp[x, y] = int.MaxValue;
            }
        }

        dp[0, 0] = 0;

        for (var i = 0; i < n; i++)
        {
            var m = medicines[i];

            for (var x = totalX; x >= m.X; x--)
            {
                for (var y = totalY; y >= m.Y; y--)
                {
                    var cost = dp[x - m.X, y - m.Y];
                    if (cost == int.MaxValue) continue;

                    dp[x, y] = Math.Min(dp[x, y], cost + m.Cost);
                }
            }
        }

        var totalCost = int.MaxValue;

        for (var x = 1; x <= Math.Min(totalX, totalY); x++)
        {
            totalCost = Math.Min(totalCost, dp[x, x]);
        }

        return totalCost == int.MaxValue ? -1 : totalCost;
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
