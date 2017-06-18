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

    string s;

    int Solve(char c)
    {
        var t = s.ToArray();

        for (var round = 1; ; round++)
        {
            var isUnique = true;

            var u = new char[t.Length - 1];
            for (var i = 0; i + 1 < t.Length; i++)
            {
                if (t[i] == c || t[i + 1] == c)
                {
                    u[i] = c;
                }
                else
                {
                    u[i] = t[i];
                    isUnique = false;
                }
            }

            if (isUnique) return round;

            t = u;
        }
    }

    int Solve()
    {
        if (s.All(c => c == s[0])) return 0;
        return s.Distinct().Select(Solve).Min();
    }

    public void Run()
    {
        s = input.ReadLine();
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
