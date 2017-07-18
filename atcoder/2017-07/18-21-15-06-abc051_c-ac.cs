using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class Condition
{
    public readonly int IntegerLowerBound, IntegerUpperBound;
    public readonly string StringLowerBound, StringUpperBound;

    public static readonly string MinString = "";
    public static readonly string MaxString = "9999999999";

    public Condition(int integerLowerBound, int integerUpperBound, string stringLowerBound, string stringUpperBound)
    {
        IntegerLowerBound = integerLowerBound;
        IntegerUpperBound = integerUpperBound;
        StringLowerBound = stringLowerBound;
        StringUpperBound = stringUpperBound;
    }

    public static Condition Yes1(int n)
    {
        return new Condition(0, n, MinString, n.ToString());
    }

    public static Condition Yes2(int n)
    {
        return new Condition(n, int.MaxValue, n.ToString(), MaxString);
    }

    public static Condition No1(int n)
    {
        return new Condition(n, int.MaxValue, MinString, n.ToString());
    }

    public static Condition No2(int n)
    {
        return new Condition(0, n, n.ToString(), MaxString);
    }

    static string Min(string left, string right)
    {
        return string.CompareOrdinal(left, right) > 0 ? right : left;
    }

    static string Max(string left, string right)
    {
        return string.CompareOrdinal(left, right) < 0 ? right : left;
    }

    public Condition And(Condition right)
    {
        return
            new Condition(
                Math.Max(IntegerLowerBound, right.IntegerLowerBound),
                Math.Min(IntegerUpperBound, right.IntegerUpperBound),
                Max(StringLowerBound, right.StringLowerBound),
                Min(StringUpperBound, right.StringUpperBound)
            );
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

    int tx, ty;

    void Read()
    {
        var l = ReadLine(int.Parse);
        var sx = l[0];
        var sy = l[1];
        tx = l[2] - sx;
        ty = l[3] - sy;
    }

    string Solve()
    {
        return
            new StringBuilder()
            .Append('U', ty)
            .Append('R', tx)
            .Append('D', ty)
            .Append('L', tx)
            .Append('L')
            .Append('U', ty + 1)
            .Append('R', tx + 1)
            .Append('D')
            .Append('R')
            .Append('D', ty + 1)
            .Append('L', tx + 1)
            .Append('U')
            .ToString();
    }

    void Verify(string s)
    {
        var x = 0;
        var y = 0;
        var k = 0;
        var l = 0;
        var done = new HashSet<Tuple<int, int>>();

        for (var i = 0; i < s.Length; i++)
        {
            switch (s[i])
            {
                case 'L':
                    x--;
                    break;
                case 'U':
                    y++;
                    break;
                case 'R':
                    x++;
                    break;
                case 'D':
                    y--;
                    break;
                default:
                    throw new Exception();
            }

            if (x == 0 && y == 0)
            {
                k++;
            }
            else if (x == tx && y == ty)
            {
                l++;
            }
            else
            {
                if (!done.Add(Tuple.Create(x, y))) throw new Exception();
            }
        }

        if (!(k == 2 && l == 2)) throw new Exception();
        if (!(x == 0 && y == 0)) throw new Exception();
    }

    public void Run()
    {
        Read();
        WriteLineOne(Solve());

        //Verify(Solve());
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
