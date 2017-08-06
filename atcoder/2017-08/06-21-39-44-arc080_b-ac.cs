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

    int h, w;
    int n;
    int[] xs;

    static readonly int[] dx = new[] { 1, 0, -1, 0 };
    static readonly int[] dy = new[] { 0, 1, 0, -1 };

    IEnumerable<IEnumerable<int>> Solve()
    {
        {
            {
                var l = ReadLine(int.Parse);
                h = l[0];
                w = l[1];
            }

            n = ReadLine(int.Parse)[0];

            xs = ReadLine(int.Parse);
        }

        var table = Enumerable.Range(0, h + 2).Select(_ => new int[w + 2]).ToArray();

        for (var y = 0; y < h + 2; y++)
        {
            for (var x = 0; x < w + 2; x++)
            {
                if (x == 0 || x == w + 1 || y == 0 || y == h + 1)
                {
                    table[y][x] = -1;
                }
            }
        }

        {
            var y = 1;
            var x = 1;
            var c = 0;
            var d = 0;

            for (var i = 0; i < h * w; i++)
            {
                table[y][x] = c + 1;
                xs[c]--;

                var y2 = y + dy[d];
                var x2 = x + dx[d];
                if (table[y2][x2] != 0)
                {
                    d = (d + 1) % 4;
                    y2 = y + dy[d];
                    x2 = x + dx[d];
                }

                y = y2;
                x = x2;

                if (xs[c] == 0)
                {
                    c++;
                }
            }
        }

        return table.Skip(1).Take(h).Select(row => row.Skip(1).Take(w));
    }

    public void Run()
    {
        var rows = Solve();

        foreach (var row in rows)
        {
            WriteLineOne(string.Join(" ", row));
        }
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
