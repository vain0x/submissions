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
    int[] js;

    void Read()
    {
        var l = ReadLine(int.Parse);
        n = l[0];
        js = Enumerable.Range(0, n).Select(_ => int.Parse(input.ReadLine()) - 1).ToArray();
    }

    int Solve()
    {
        var done = new bool[n];

        var i = 0;
        var d = 1;

        while (true)
        {
            if (done[i]) return -1;
            done[i] = true;

            var j = js[i];
            if (j == 1) return d;

            i = j;
            d++;
        }
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
