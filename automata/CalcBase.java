import java.io.Console;
import java.util.Stack;


/** Base class used by the generated Calc. **/
public abstract class CalcBase {
    public Stack<Integer> stack = new Stack<Integer>();

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
		stack.push(Integer.valueOf(input));
	    } catch (NumberFormatException e) {
		printTop = dispatch(input);
	    }
	    if (printTop) {
		System.out.println("... " + stack.peek());
	    }
	}
    }
}
