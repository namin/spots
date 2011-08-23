import java.io.Console;
import java.math.BigInteger;
import java.util.Stack;


/** Base class used by the generated Calc. **/
public abstract class CalcBase {
    public Stack<BigInteger> stack = new Stack<BigInteger>();

    public abstract boolean dispatch(String op);

    public void readEvalLoop() {
	while (true) {
	    String input = System.console().readLine();
	    if (input == null) {
		break;
	    }
	    input = input.trim();
	    boolean printTop = true;
	    try {
		stack.push(new BigInteger(input));
	    } catch (NumberFormatException e) {
		printTop = dispatch(input);
	    }
	    if (printTop) {
		System.out.println("... " + stack.peek());
	    }
	}
    }
}
