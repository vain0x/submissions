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

public static class PermutationAlgorithm
{
    private static void InplaceSwap<X>(IList<X> list, int i, int j)
    {
        var t = list[i];
        list[i] = list[j];
        list[j] = t;
    }

    private static void InplaceReverse<X>(IList<X> list, int first, int count)
    {
        for (var i = 0; i < count / 2; i++)
        {
            InplaceSwap(list, first + i, first + count - 1 - i);
        }
    }

    /// <summary>
    /// Rearranges items into the next lexicographically greater permutation.
    /// Returns <c>true</c> if rearranged.
    /// </summary>
    public static bool NextPermutation<X>(this IList<X> list, IComparer<X> comparer)
    {
        var count = list.Count;
        if (count <= 1) return false;

        var i = count - 1;
        while (true)
        {
            var j = i;

            i--;

            if (comparer.Compare(list[i], list[j]) < 0)
            {
                var k = count - 1;
                while (comparer.Compare(list[i], list[k]) >= 0)
                {
                    k--;
                }

                InplaceSwap(list, i, k);
                InplaceReverse(list, j, count - j);
                return true;
            }

            if (i == 0)
            {
                InplaceReverse(list, 0, count);
                return false;
            }
        }
    }

    /// <summary>
    /// Rearranges items into the next lexicographically greater permutation
    /// in the default order.
    /// Returns <c>true</c> if rearranged.
    /// </summary>
    public static bool NextPermutation<X>(this IList<X> list)
    {
        return NextPermutation(list, Comparer<X>.Default);
    }

    /// <summary>
    /// Generates all permutations.
    /// </summary>
    public static IEnumerable<IReadOnlyList<X>> Permutations<X>(this IEnumerable<X> xs, IComparer<X> comparer)
    {
        var array = xs.ToArray();
        Array.Sort(array, comparer);

        do
        {
            yield return array;
        }
        while (NextPermutation(array, comparer));
    }

    /// <summary>
    /// Generates all permutations.
    /// </summary>
    public static IEnumerable<IReadOnlyList<X>> Permutations<X>(this IEnumerable<X> xs)
    {
        return Permutations(xs, Comparer<X>.Default);
    }
}

public class Generator
{
    public bool[][] Candidates(int a, int b)
    {
        var t = Enumerable.Repeat(false, a).Concat(Enumerable.Repeat(true, b)).ToArray();
        return t.Permutations().Select(xs => xs.ToArray()).ToArray();
    }

    int Limit(bool[] xs)
    {
        var l = 1;
        var x = xs[0];
        var s = 1;
        for (var i = 1; i < xs.Length; i++)
        {
            if (xs[i] == x)
            {
                s++;
            }
            else
            {
                l = Math.Max(l, s);
                s = 1;
                x = xs[i];
            }
        }
        l = Math.Max(l, s);
        return l;
    }

    int ToInt(bool[] b)
    {
        var n = 0;
        for (var i = 0; i < b.Length; i++)
        {
            n = n << 1;

            if (b[i])
            {
                n = n | 1;
            }
        }
        return n;
    }

    public string F(int a, int b)
    {
        var s = Candidates(a, b).OrderBy(xs => Limit(xs)).ThenBy(t => ToInt(t)).First();
        return new string(s.Select(x => x ? 'B' : 'A').ToArray());
    }

    public void Run()
    {
        for (var a = 1; a < 10; a++)
        {
            for (var b = 1; b < 10; b++)
            {
                Console.WriteLine("A={0:D2} B={1:D2} f={2}", a, b, F(a, b));
            }
        }
    }
}

public class Solver
{
    int A, B;
    StringBuilder sb = new StringBuilder();

    string SolveCore(int limit)
    {
        var Al = (A + limit - 1) / limit;
        var Bl = (B + limit - 1) / limit;

        var solve = new Func<int, int, string>((Ap, Bp) =>
        {
            if (!(Ap <= A && Bp <= B && Ap <= B + 1 && Bp <= A + 1))
            {
                return null;
            }

            var Ak = A / Ap;
            var Bk = B / Bp;

            var a = Ap >= Bp;
            var ai = 0;
            var bi = 0;

            sb.Clear();
            for (var i = 0; i < Ap + Bp; i++)
            {
                if (a)
                {
                    // 最初の A%Ap 回のAパートは1長い
                    var k = Ak + (ai < (A % Ap) ? 1 : 0);
                    sb.Append('A', k);
                    ai++;
                }
                else
                {
                    var k = Bk + (Bp - bi - 1 < (B % Bp) ? 1 : 0);
                    sb.Append('B', k);
                    bi++;
                }

                a = !a;
            }
            return sb.ToString();
        });

        var p = Math.Max(Al, Bl);

        string s;
        s = solve(p, p);
        if (s != null) return s;
        s = solve(p - 1, p);
        if (s != null) return s;
        s = solve(p, p - 1);
        if (s != null) return s;
        return null;
    }

    public string Solve()
    {
        for (var l = 1; ; l++)
        {
            var s = SolveCore(l);
            if (s != null)
            {
                return s;
            }
        }

        throw new Exception();
    }

    public Solver(int a, int b)
    {
        A = a;
        B = b;
    }
}

public class Checker
{
    Generator gen = new Generator();

    public void Check(int a, int b)
    {
        var actual = new Solver(a, b).Solve();
        var expected = gen.F(a, b);
        if (actual != expected)
        {
            Console.Error.WriteLine("A={0} B={1}", a, b);
            Console.Error.WriteLine("Ac={0}", actual);
            Console.Error.WriteLine("Ex={0}", expected);
        }
    }

    public void Run()
    {
        for (var a = 1; a < 10; a++)
        {
            for (var b = 1; b < 10; b++)
            {
                Check(a, b);
            }
        }
    }
}

public sealed partial class Program
{

    public void EntryPoint()
    {
        // new Checker().Check(1, 2);
        // new Checker().Run(); return;
        // new Generator().Run(); return;

        var I = _scanner;
        var Q = I.N();
        var As = new int[Q];
        var Bs = new int[Q];
        var Cs = new int[Q];
        var Ds = new int[Q];

        for (var q = 0; q < Q; q++)
        {
            As[q] = I.N();
            Bs[q] = I.N();
            Cs[q] = I.N();
            Ds[q] = I.N() + 1;

            // 部分点狙い
            if (As[q] > 1000 || Bs[q] > 1000)
            {
                return;
            }
        }

        for (var q = 0; q < Q; q++)
        {
            var f = new Solver(As[q], Bs[q]).Solve();
            WriteLine(f.Substring(Cs[q], Ds[q] - Cs[q]));
        }
    }
}
