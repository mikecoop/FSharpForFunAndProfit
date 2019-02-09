using System;
using System.Collections.Generic;
using static System.Console;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpExamples
{
    public class LinqExamples
    {
        public void Test()
        {
            int square(int x) => x * x;
            int sumOfSquares(int n) => Enumerable.Range(1, n).Select(square).Sum();

            WriteLine(sumOfSquares(100));

            int s2 = square(2);
            int s3 = square(3);
            int s4 = square(4);

            ////////////////////////////////////////////////////////////////

            Func<int, int> squareClone = square;

            int execFunction(Func<int, int> aFunc, int aParam) => aFunc(aParam);
            int result = execFunction(squareClone, 12);

            ////////////////////////////////////////////////////////////////

            int product(int n) => Enumerable.Range(1, n).Aggregate(1, (acc, val) => acc * val);
            WriteLine(product(10));

            int sumOfOdds(int n) => Enumerable.Range(1, n).Where(x => x % 2 == 1).Sum();
            WriteLine(sumOfOdds(10));

            int alternatingSum(int n) => Enumerable.Range(1, n)
                                         .Select((x, i) => i % 2 == 0 ? -x : x)
                                         .Sum();
            WriteLine(alternatingSum(10));

            int alternatingSum2(int n) => (Enumerable.Range(1, n)
                                          .Aggregate(
                                          (true, 0),
                                          ((bool neg, int sumSoFar) tuple, int x) => 
                                          tuple.neg ? (false, tuple.sumSoFar - x) : (true, tuple.sumSoFar + x))).Item2;
            WriteLine(alternatingSum2(10));

            ////////////////////////////////////////////////////////////////
            

        }
    }
}
