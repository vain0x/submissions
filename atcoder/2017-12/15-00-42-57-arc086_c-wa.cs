using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public static class TemplateExtension
{
    public static X[] MakeArray<X>(this int count, Func<int, X> func)
    {
        var xs = new X[count];
        for (var i = 0; i < count; i++)
        {
            xs[i] = func(i);
        }
        return xs;
    }

    public static int[] Range(this int count, int start = 0)
    {
        return count.MakeArray(i => i + start);
    }

    public static string Intercalate<X>(this IEnumerable<X> @this, string separator)
    {
        return string.Join(separator, @this);
    }

    public sealed class ValueIndexPair<T>
        : Tuple<T, int>
    {
        public T Value { get { return Item1; } }
        public int Index { get { return Item2; } }

        public ValueIndexPair(T value, int index)
            : base(value, index)
        {
        }
    }

    public static IEnumerable<ValueIndexPair<X>> Indexed<X>(this IEnumerable<X> @this)
    {
        var i = 0;
        foreach (var x in @this)
        {
            yield return new ValueIndexPair<X>(x, i);
            i++;
        }
    }
}

public sealed class Scanner
{
    private readonly TextReader _reader;
    private readonly StringBuilder _sb = new StringBuilder();

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        _sb.Clear();

        while (true)
        {
            var r = _reader.Read();

            if (r == '\r')
            {
                if (_reader.Peek() == '\n') _reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
            {
                break;
            }
            else
            {
                _sb.Append((char)r);
            }
        }

        return _sb.ToString();
    }

    /// <summary>
    /// Reads next word as <see cref="int"/>.
    /// </summary>
    public int N()
    {
        return int.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="long"/>.
    /// </summary>
    public long L()
    {
        return long.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="double"/>.
    /// </summary>
    public double F()
    {
        return double.Parse(Word());
    }

    public int[] Ns(int count)
    {
        return count.MakeArray(_ => N());
    }

    public long[] Ls(int count)
    {
        return count.MakeArray(_ => L());
    }

    public double[] Fs(int count)
    {
        return count.MakeArray(_ => F());
    }

    /// <summary>
    /// Reads next line and splits it by spaces.
    /// </summary>
    public X[] Words<X>(Func<string, X> func)
    {
        return _reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        _reader = reader;
    }
}

public partial class Program
{
    private readonly TextReader _input;
    private readonly TextWriter _output;
    private readonly Scanner _scanner;

    private void WriteLine(int value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(long value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(double value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(char value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(string value)
    {
        _output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        _input = input;
        _output = output;
        _scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).EntryPoint();
    }
}

public sealed partial class Program
{
    // 解説を読んで問題を解く。
    // 解説だけだと分からなかったのでこの提出も参考にした: <https://beta.atcoder.jp/contests/arc086/submissions/1860264>

    // 語彙:
    // ビー玉が配置された頂点の深さを、ビー玉の深さと呼ぶ。
    // ビー玉の配置 S から、深さが d でない頂点に置かれたビー玉をすべて取り除いた配置を S(d) とおく。
    // ビー玉の配置 S を初期状態としてゲームを行ったときに箱に入るビー玉の個数を f(S) と置く。
    // 問題文では1つのノードに複数のビー玉を移動するとき、移動してから消滅させているが、ここでは移動前に消滅することにする。

    // 考察1: 
    // ゲーム中、個々のビー玉が箱に到達するか消滅するかは、同じ深さのビー玉がどのように配置されたかにのみ依存する。
    // このとき、 f(S) = sum { f(S(d)) | d } である。
    // したがって、ビー玉の深さがすべて等しい配置のみ計算すればよい。

    const long P = 1000000007;

    int N;
    int[] p;

    List<int>[] children;

    /// widths[d] = k ⇔ 深さ d の頂点が k 個ある。
    int[] widths;

    void CalculateWidths(int v, int d)
    {
        foreach (var c in children[v])
        {
            CalculateWidths(c, d + 1);
        }

        widths[d]++;
    }

    long Power(long x, int n)
    {
        // 繰り返し二乗法

        var acc = 1L;
        while (n != 0)
        {
            if ((n & 1) != 0)
            {
                acc = (acc * x) % P;
            }

            n = (n >> 1);
            x = (x * x) % P;
        }
        return acc;
    }

    /// dp[v][s] ≡ k
    /// (深さ d を1つ固定した状態で、) ツリー全体における深さ d の頂点の集合にビー玉を配置する方法のうち、
    /// ゲーム中に頂点 v にビー玉が乗る (s = 1) or 乗らない (s = 0) ものの個数が k に等しい。
    /// (Dfs(v, _) の後に dp[v][_] を読むだけなので、配列は必要ないはず。)
    long[][] dp;

    // dp 表を埋めていく。
    void Dfs(int v, int d)
    {
        if (d == 0)
        {
            // このとき、頂点 v にビー玉を置く場合と、置かない場合の2通りを考える。前者ならビー玉は箱に入り、後者なら入らない。
            dp[v][0] = 1;
            dp[v][1] = 1;
            return;
        }

        var a = Tuple.Create(1L, 0L, 0L);

        // 子ノード c_i にビー玉が乗る配置の総数を p_i、乗らない配置の総数を q_i とするとき、
        // (p_i + q_i) の相乗のうち p_i がちょうど1つだけ出現する項の総和が v にビー玉が乗る配置の総数を表し、
        // 残りの項の総和が v にビー玉が乗らない配置の総数を表す。
        // 項の分類は後述の行列の冪を (1, 0, 0) にかけるとスマート。

        foreach (var c in children[v])
        {
            Dfs(c, d - 1);

            var p = dp[c][1];
            var q = dp[c][0];
            var r = (p + q) % P;

            // 不思議な行列をかける。
            // [ q, 0, 0        
            // ; p, q, 0
            // ; 0, p, r ]
            a = Tuple.Create(
                (q * a.Item1) % P,
                ((p * a.Item1) % P + (q * a.Item2) % P) % P,
                ((p * a.Item2) % P + (r * a.Item3) % P) % P
            );
        }

        dp[v][0] = (a.Item1 + a.Item3) % P;
        dp[v][1] = a.Item2;
    }

    private long Solve()
    {
        // 部分点のみ狙う。
        if (N >= 2000) return -1;

        children = (N + 1).MakeArray(i => new List<int>());
        for (var i = 0; i < N; i++)
        {
            children[p[i]].Add(i + 1);
        }

        widths = (N + 1).MakeArray(i => 0);
        CalculateWidths(0, 0);

        var sum = 0L;
        for (var d = 0; d < N + 1; d++)
        {
            if (widths[d] == 0) break;

            dp = (N + 1).MakeArray(i => 2.MakeArray(j => 0L));

            Dfs(0, d);

            var k = dp[0][1];

            // 深さ d のビー玉だけを置くサブゲームに分解されるゲームはたくさんあるので、係数をかける。
            k = (k * Power(2, (N + 1) - widths[d])) % P;

            sum = (sum + k) % P;
        }
        return sum;
    }

    private void Read()
    {
        var a = _scanner;
        N = a.N();
        p = N.MakeArray(i => a.N());
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
