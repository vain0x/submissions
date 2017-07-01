using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class CombinationFunction
{
    readonly int[,] dp;

    public int Invoke(int n, int k)
    {
        return dp[n, k];
    }

    public CombinationFunction(int m, int mod)
    {
        dp = new int[m + 1, m + 1];

        for (var n = 0; n <= m; n++)
        {
            for (var k = 0; k <= m; k++)
            {
                dp[n, k] =
                    k == 0 ? 1 :
                    k == 1 ? n :
                    k > n ? 0 :
                    (n - k) < k ? dp[n, n - k] :
                    (dp[n - 1, k - 1] + dp[n - 1, k]) % mod;
            }
        }
    }
}

public class Program
{
    public const int Mod = 1000000007;

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

    IEnumerable<long> Solve()
    {
        yield return n;

        var combination = new CombinationFunction(n, Mod);

        // 長さ n+1 の数列に n 個の数値が1つ以上出現するということは、
        // ちょうど1つの数値が2回出現する。
        // 重複する数値で数列を3つ (P, Q, R) に分割し、それぞれの長さを p, q, r とおく。
        var p = default(int);
        var q = default(int);
        var r = default(int);
        {
            // 数値 i が ys[i] 番目に初めて出現する。
            var ys = Enumerable.Repeat(-1, n + 2).ToArray();
            for (var i = 0; i < n; i++)
            {
                var j = ys[xs[i]];
                if (j == -1)
                {
                    ys[xs[i]] = i;
                }
                else
                {
                    p = j;
                    q = i - j - 1;
                    r = (n + 1) - i - 1;
                    break;
                }
            }
        }

        for (var k = 2; k <= n; k++)
        {
            // 重複する数値が出現しないケース。
            // PQR
            var c = 0L;

            c = combination.Invoke(p + q + r, k);
            // 重複する数値が1つだけ出現するケース。
            // P1QR + PQ1R - P1R
            c = (c + combination.Invoke(p + q + r, k - 1)) % Mod;
            c = (c + combination.Invoke(p + q + r, k - 1)) % Mod;
            c = (c - combination.Invoke(p + r, k - 1) + Mod) % Mod;
            // 重複する数値が2つ出現するケース。
            // P1Q1R
            c = (c + combination.Invoke(p + q + r, k - 2)) % Mod;
            yield return c;
        }

        yield return 1;
    }

    public void Run()
    {
        Read();

        foreach (var c in Solve())
        {
            WriteLineOne(c);
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
