using System;
using System.Linq;

namespace CSharpExamples
{
    interface ICalculator
    {
        int Calculate(int input);
    }

    class AddingCalculator : ICalculator
    {
        public int Calculate(int input) => input + 1;
    }
    
    class LoggingCalculator : ICalculator
    {
        ICalculator calculator;

        LoggingCalculator(ICalculator calculator) => this.calculator = calculator;

        public int Calculate(int input)
        {
            Console.WriteLine($"input is {input}");
            var result = calculator.Calculate(input);
            Console.WriteLine($"result is {result}");
            return result;
        }
    }

    class WithFunctions
    {
        Func<int, int> calculator;

        void Test()
        {
            int addingCalculator(int n) => n + 1;

            calculator = addingCalculator;
        }
    }
}
