using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class CombinationFunction
{
    readonly long[,] dp;

    public long Invoke(int n, int k)
    {
        return dp[n, k];
    }

    public CombinationFunction(int m)
    {
        dp = new long[m + 1, m + 1];

        for (var n = 0; n <= m; n++)
        {
            for (var k = 0; k <= m; k++)
            {
                dp[n, k] =
                    k == 0 ? 1 :
                    k == 1 ? n :
                    k > n ? 0 :
                    (n - k) < k ? dp[n, n - k] :
                    dp[n - 1, k - 1] + dp[n - 1, k];
            }
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

    int n, a, b;
    long[] vs;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        a = line[1];
        b = line[2];
        vs = ReadLine(long.Parse);

        Array.Sort(vs, (l, r) => Comparer<long>.Default.Compare(r, l));
    }

    long Combination()
    {
        var combination = new Func<int, int, long>(new CombinationFunction(n).Invoke);

        var kvs =
            vs.GroupBy(v => v)
            .Select(g => new KeyValuePair<long, int>(g.Key, g.Count()))
            .OrderByDescending(kv => kv.Key)
            .ToArray();

        // Invariant: count < a.
        var count = 0;

        foreach (var kv in kvs)
        {
            // Value.
            var v = kv.Key;

            // Multiplicity.
            var m = kv.Value;

            if (count + m < a)
            {
                // 価値 v の品物を m 個すべて選ぶ。選択肢はない。
                count += m;
                continue;
            }

            if (count == 0)
            {
                // 価値 v の品物をいくつか選ぶことで平均を最大化できる。
                // 必要以上に選んでも最大になる。
                // これより後にある (価値の低い) 品物を選ぶことでは最大化されない。
                return Enumerable.Range(a, b - a + 1).Sum(k => combination(m, k));
            }
            else
            {
                // 平均が最大化されるのは、これまでに選んだ高価値の品物と、
                // 最小限の価値 v の品物を選んだ場合のみ。
                return combination(m, a - count);
            }
        }

        // count = n < a; ありえない。
        throw new Exception();
    }

    void Solve(out double maxAverage, out long combination)
    {
        maxAverage = vs.Take(a).Average();
        combination = Combination();
    }

    public void Run()
    {
        Read();

        var maxAverage = default(double);
        var combination = default(long);
        Solve(out maxAverage, out combination);
        WriteLineOne(maxAverage.ToString("F7"));
        WriteLineOne(combination);
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
