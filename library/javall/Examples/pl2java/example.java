import java.awt.*;
import java.awt.event.*;

class example implements ActionListener {
  TextField display;
  boolean number_editing = false;
  String acumulator = "0";
  String operator = "+";

  public static void main(String argv[]) {
    example ex;
    ex = new example();
  }

  public example() {
    Frame frame = new Frame("Prueba");

    frame.resize(300,300);
    frame.setLocation(1,1);

    BorderLayout layout = new BorderLayout();
    frame.setLayout(layout);

    display = new TextField();
    frame.add("North",display);

    Panel btnPanel = new Panel();
    GridLayout btnLayout = new GridLayout(4,4);
    Button btn1 = new Button("1");
    Button btn2 = new Button("2");
    Button btn3 = new Button("3");
    Button btn4 = new Button("4");
    Button btn5 = new Button("5");
    Button btn6 = new Button("6");
    Button btn7 = new Button("7");
    Button btn8 = new Button("8");
    Button btn9 = new Button("9");
    Button btn0 = new Button("0");
    Button btnAdd = new Button("+");
    Button btnMult = new Button("*");
    Button btnDiv = new Button("/");
    Button btnSub = new Button("-");
    Button btnEquals = new Button("=");
    Button btnEnd = new Button("Exit");

    frame.add("Center",btnPanel);
    btnPanel.setLayout(btnLayout);
    btnPanel.add(btn1);
    btnPanel.add(btn2);
    btnPanel.add(btn3);
    btnPanel.add(btnAdd);
    
    btnPanel.add(btn4);
    btnPanel.add(btn5);
    btnPanel.add(btn6);
    btnPanel.add(btnSub);
    
    btnPanel.add(btn7);
    btnPanel.add(btn8);
    btnPanel.add(btn9);
    btnPanel.add(btnMult);
    
    btnPanel.add(btnEnd);
    btnPanel.add(btn0);
    btnPanel.add(btnEquals);
    btnPanel.add(btnDiv);

    btn1.addActionListener(this);
    btn2.addActionListener(this);
    btn3.addActionListener(this);
    btn4.addActionListener(this);
    btn5.addActionListener(this);
    btn6.addActionListener(this);
    btn7.addActionListener(this);
    btn8.addActionListener(this);
    btn9.addActionListener(this);
    btn0.addActionListener(this);
    btnAdd.addActionListener(this);
    btnSub.addActionListener(this);
    btnMult.addActionListener(this);
    btnDiv.addActionListener(this);
    btnEquals.addActionListener(this);
    btnEnd.addActionListener(this);

    frame.show();
  }

  public void actionPerformed(ActionEvent e) {
    if (e.getSource() instanceof Button) {
      String label = ((Button)e.getSource()).getLabel();
      if (label.equals("1") ||
	  label.equals("2") ||
	  label.equals("3") ||
	  label.equals("4") ||
	  label.equals("5") ||
	  label.equals("6") ||
	  label.equals("7") ||
	  label.equals("8") ||
	  label.equals("9") ||
	  label.equals("0")) {
	if (number_editing)
	  display.setText(display.getText() + label);
	else {
	  display.setText(label);
	  number_editing = true;
	}
      }
      
      if (label.equals("=") ||
	  label.equals("+") ||
	  label.equals("-") ||
	  label.equals("/") ||
	  label.equals("*")){
	performCalculation(operator, acumulator, display.getText());
	operator = label;
      }

      if (label.equals("Exit"))
	System.exit(0);
    }
  }

  private void performCalculation(String operator,
				  String operand1,
				  String operand2) {

    long op1 = Long.valueOf(operand1).longValue(); 
    long op2 = Long.valueOf(operand2).longValue(); 
    long result = 0;

    if (operator.equals("="))
      result = op2;

    if (operator.equals("+"))
      result = op1 + op2;

    if (operator.equals("-"))
      result = op1 - op2;

    if (operator.equals("*"))
      result = op1 * op2;

    if (operator.equals("/") && (op2 != 0))
      result = op1 / op2;

    acumulator = String.valueOf(result);
    display.setText(acumulator);
    number_editing = false;
  }
}


	


