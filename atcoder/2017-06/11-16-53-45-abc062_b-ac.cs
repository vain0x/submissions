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

    public void Run()
    {
        var firstLine = ReadLine(int.Parse);
        var h = firstLine[0];
        var w = firstLine[1];
        var image = Enumerable.Range(0, h).Select(_ => input.ReadLine()).ToArray();

        var borderLine = new string('#', w + 2);
        var lines =
            new[] { borderLine }
            .Concat(image.Select(line => "#" + line + "#"))
            .Concat(new[] { borderLine })
            .ToArray();

        foreach (var line in lines)
        {
            output.WriteLine(line);
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
