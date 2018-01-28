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

            if (r == ' ' || r == '\r' || r == '\n')
            {
                if (r == '\r' && _reader.Peek() == '\n')
                {
                    _reader.Read();
                }

                // Ignore leading spaces.
                if (_sb.Length == 0) continue;

                break;
            }
            else if (r == -1)
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
        return int.Parse(Word().Trim());
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

public struct Var
{
    public int V;
    public long D;
}

public sealed partial class Program
{
    int N, M;
    int[] L;
    int[] R;
    long[] D;

    Var[] m;

    Var Solve(int ni)
    {
        if (m[ni].V == ni)
        {
            return m[ni];
        }

        var d = m[ni].D;
        var v = Solve(m[ni].V);
        var w = new Var { V = v.V, D = v.D + d };
        m[ni] = w;
        return w;
    }

    // 変数niの値を x_nj + d にする
    void Set(int ni, int nj, long d)
    {
        var jv = Solve(nj);
        m[ni] = new Var()
        {
            V = jv.V,
            D = d,
        };
    }

    private bool Solve()
    {
        m = new Var[N];
        for (var ni = 0; ni < N; ni++)
        {
            m[ni] = new Var()
            {
                V = ni,
                D = 0,
            };
        }

        for (var mi = 0; mi < M; mi++)
        {
            var l = L[mi];
            var r = R[mi];
            var d = D[mi];

            var lv = m[l];
            var rv = m[r];
            // x_l = (x_l' + d1)
            // x_r = (x_r' + d2)
            // x_r - x_l = d

            // x_l' = (x_r') + (d2 - d1 - d)
            Set(lv.V, rv.V, rv.D - lv.D - d);
        }

        var ok = true;
        for (var ni = 0; ni < N; ni++)
        {
            var v = Solve(ni);
            if (v.V == ni && v.D != 0)
            {
                ok = false;
                break;
            }
        }

        return ok;
    }

    public void EntryPoint()
    {
        var I = _scanner;
        N = I.N();
        M = I.N();
        L = new int[M];
        R = new int[M];
        D = new long[M];

        for (var i = 0; i < M; i++)
        {
            L[i] = I.N() - 1;
            R[i] = I.N() - 1;
            D[i] = I.L();
        }

        WriteLine(Solve() ? "Yes" : "No");
    }
}
