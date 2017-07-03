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

    int n, ma, mb;
    int[] xs1, ys1, cs;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        ma = line[1];
        mb = line[2];

        xs1 = new int[n];
        ys1 = new int[n];
        cs = new int[n];
        for (var i = 0; i < n; i++)
        {
            var l = ReadLine(int.Parse);
            xs1[i] = l[0];
            ys1[i] = l[1];
            cs[i] = l[2];
        }
    }

    Medicine[] Combine(ArraySegment<Medicine> medicines)
    {
        var n = 1 << medicines.Count;

        var combines = new Medicine[n];

        for (var i = 0; i < n; i++)
        {
            var x = 0;
            var y = 0;
            var cost = 0;

            for (var j = 0; j < medicines.Count; j++)
            {
                if ((i & (1 << j)) == 0) continue;

                var m = medicines.Array[medicines.Offset + j];
                x += m.X;
                y += m.Y;
                cost += m.Cost;
            }

            combines[i] = new Medicine()
            {
                X = x,
                Y = y,
                Cost = cost,
            };
        }

        return combines;
    }

    int Solve()
    {
        var medicines = new Medicine[n];
        for (var i = 0; i < n; i++)
        {
            medicines[i].X = mb * xs1[i];
            medicines[i].Y = ma * ys1[i];
            medicines[i].Cost = cs[i];
        }

        // 前半の薬品のすべての組み合わせ。(≦ 2^20 ≦ 10^7)
        var left = Combine(new ArraySegment<Medicine>(medicines, 0, n / 2));

        // 後半の薬品のすべての組み合わせ。
        var right = Combine(new ArraySegment<Medicine>(medicines, n / 2, n - n / 2));

        var costsFromDiff = new Dictionary<int, List<int>>();
        for (var i = 0; i < right.Length; i++)
        {
            var key = right[i].X - right[i].Y;

            var list = default(List<int>);
            if (!costsFromDiff.TryGetValue(key, out list))
            {
                list = new List<int>();
                costsFromDiff.Add(key, list);
            }

            list.Add(right[i].Cost);
        }

        foreach (var kv in costsFromDiff)
        {
            kv.Value.Sort();
        }

        var totalCost = int.MaxValue;

        for (var i = 0; i < left.Length; i++)
        {
            var d = left[i].Y - left[i].X;

            var rightCosts = default(List<int>);
            if (costsFromDiff.TryGetValue(d, out rightCosts))
            {
                for (var j = 0; j < Math.Min(2, rightCosts.Count); j++)
                {
                    var cost = left[i].Cost + rightCosts[j];
                    if (cost == 0) continue;

                    totalCost = Math.Min(totalCost, cost);
                }
            }
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
