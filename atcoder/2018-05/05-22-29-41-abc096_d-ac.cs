using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;
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
        _output.WriteLine(value.ToString(CultureInfo.InvariantCulture));
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
    bool[] Sieve;
    int[] Primes;

    const int MAX = 55555 + 1;

    int[] xs = new int[5];
    //int[] ps = new int[55];

    bool IsPrime(int p)
    {
        if (p <= 1) return false;
        for (var q = 2; q * q <= p; q++)
        {
            if (p % q == 0) return false;
        }
        return true;
    }

    bool IsOk(int[] ps, int[] xs, int xi, int pi, int sum)
    {
        if (xi == xs.Length)
        {
            return !IsPrime(sum); // !Sieve[sum]; // !IsPrime(sum);
        }

        for (var pj = pi; pj < ps.Length; pj++)
        {
            xs[xi] = ps[pj];
            if (!IsOk(ps, xs, xi + 1, pj + 1, sum + ps[pj]))
            {
                return false;
            }
        }
        return true;
    }

    int[] FindRound11(int[] qs)
    {
        var result = new List<int>();
        var qi = 0;

        for (var r = 0; r < 11; r++)
        {
            var ps = qs.Skip(qi).Take(4);
            var sum = ps.Sum();
            var qj = qi + 4;
            for (; qj < qs.Length; qj++)
            {
                if (Sieve[sum + qs[qj]]) continue;
                break;
            }
            if (qj == qs.Length) throw new Exception();

            result.AddRange(ps);
            qi = qj + 1;
        }
        return result.ToArray();
    }

    int[] FindMod5(int[] qs)
    {
        return qs.Where(q => q % 5 == 1).Take(55).ToArray();
    }

    void Build()
    {
        {
            var primes = new List<int>();

            Sieve = (MAX * 5 + 1).MakeArray(_ => true);
            Sieve[1] = false;

            for (var p = 2; p <= MAX; p++)
            {
                if (!Sieve[p]) continue;

                primes.Add(p);

                var m = p * 2;
                while (m < Sieve.Length)
                {
                    Sieve[m] = false;
                    m += p;
                }
            }

            Primes = primes.ToArray();

            Console.Error.WriteLine(string.Join(", ", Primes));
        }


        var ps = FindMod5(Primes);
        Console.Error.WriteLine(string.Join(", ", ps));
        Console.Error.WriteLine(IsOk(ps, xs, 0, 0, 0));
    }

    void Solve()
    {
        var I = _scanner;
        var ps = new[]
        {
            11, 31, 41, 61, 71, 101, 131, 151, 181, 191, 211, 241, 251, 271, 281, 311, 331, 401, 421, 431, 461, 491, 521, 541, 571, 601, 631, 641, 661, 691, 701, 751, 761, 811, 821, 881, 911, 941, 971, 991, 1021, 1031, 1051, 1061, 1091, 1151, 1171, 1181, 1201, 1231, 1291, 1301, 1321, 1361, 1381
        };

        var N = I.N();
        WriteLine(ps.Take(N).Intercalate(" "));
    }

    public void EntryPoint()
    {
        // Build();
        Solve();
    }
}
