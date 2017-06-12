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
    long weight;
    long[] ws;
    long[] vs;

    /// <summary>
    /// 重さの合計が weight を超えない範囲で
    /// 与えられた n 個のものを最大 r 個まで選び、
    /// 価値の合計を最大化するときの、最大値を計算する。
    /// </summary>
    public static long Knapsack(int n, int r, int weight, int[] ws, long[] vs)
    {
        // dp[i, j, w] = v ⇔
        // 前から i 個のものを、最大 j 個まで選ぶ、
        // 合計が w 以下になる選び方の、最大の価値が v である。
        var dp = new long[n + 1, r + 1, weight + 1];

        for (var i = 0; i <= n; i++)
        {
            for (var j = 0; j <= r; j++)
            {
                //for (var w = weight; w >= 0; w--)
                for (var w = 0; w <= weight; w++)
                {
                    if (i < n && j < r)
                    {
                        if (w + ws[i] <= weight)
                        {
                            dp[i + 1, j + 1, w + ws[i]] =
                                Math.Max(dp[i + 1, j + 1, w + ws[i]], dp[i, j, w] + vs[i]);
                        }

                        dp[i + 1, j + 1, w] = Math.Max(dp[i + 1, j + 1, w], dp[i, j, w]);
                    }

                    if (i < n)
                    {
                        dp[i + 1, j, w] = Math.Max(dp[i + 1, j, w], dp[i, j, w]);
                    }

                    if (j < r)
                    {
                        dp[i, j + 1, w] = Math.Max(dp[i, j + 1, w], dp[i, j, w]);
                    }
                }
            }
        }

        return Enumerable.Range(0, weight + 1).Select(w => dp[n, r, w]).Max();
    }

    public void Run()
    {
        {
            var firstLine = ReadLine(long.Parse);
            n = (int)firstLine[0];
            weight = firstLine[1];

            ws = new long[n];
            vs = new long[n];
            for (var i = 0; i < n; i++)
            {
                var line = ReadLine(long.Parse);

                ws[i] = line[0];
                vs[i] = line[1];
            }
        }

        var maxValue = long.MinValue;
        var us = ws.Select(w => (int)(w - ws[0])).ToArray();
        var totalRestWeights = us.Sum();
        var sortedValues = vs.OrderByDescending(v => v).ToArray();

        for (var r = 0; r <= n; r++)
        {
            var rest = weight - r * ws[0];
            if (rest < 0) continue;
            if (rest >= totalRestWeights)
            {
                maxValue = Math.Max(maxValue, new ArraySegment<long>(sortedValues, 0, r).Sum());
                continue;
            }

            // rest < total <= 3 * (n - 1)

            maxValue = Math.Max(maxValue, Knapsack(n, r, (int)rest, us, vs));
        }

        WriteLineOne(maxValue);
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
