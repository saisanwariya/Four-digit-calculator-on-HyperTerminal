# HyperTerminal Calculator

## Overview

This project involves creating an elementary calculator program that runs on the HyperTerminal connected to the HCS12 board. The calculator program can perform basic arithmetic operations on positive decimal integers and display the results.

## Program Functionality

The calculator program follows these rules:

- Input positive decimal integer numbers only.
- Input and output maximum four-digit numbers only.
- Valid operators are: +, -, *, and /
- Input numbers with leading zeros are accepted.
- Input only two numbers and one operator in between, with no spaces.
- Display 'Ecalc> ' prompt and echo user keystrokes until the Return key is pressed.
- Display user input and the result after the '=' sign.
- In case of an invalid input format, repeat the user input until the error character.
- In case of an invalid input format, print an error message on the next line: 'Invalid input format.'
- Keep 16-bit internal binary number format, detect and flag overflow error.
- Use integer division and truncate any fraction.

## Hardware Setup

To run this program on a CodeWarrior HSC12 board, ensure that you have the following hardware components set up:

- CodeWarrior HSC12 development board
- Serial communication interface (HyperTerminal or equivalent)

## Software Setup

Before running the program, you need to set up the software environment:

1. Install CodeWarrior software on your computer if you haven't already.

2. Connect the CodeWarrior HSC12 board to your computer using the appropriate communication interface.

3. Ensure that the HyperTerminal or a similar terminal emulator is set up to communicate with the HCS12 board via the serial port.

4. Compile and assemble the program using CodeWarrior to generate the binary file.

## Running the Program

To run the program on the CodeWarrior HSC12 board, follow these steps:

1. Load the compiled binary file onto the HCS12 board.

2. Power on the HCS12 board.

3. Open HyperTerminal or the terminal emulator configured for serial communication.

4. Ensure that the baud rate and serial port settings match the settings used in the program.

5. You should see the 'Ecalc> ' prompt on the terminal.

6. Enter valid expressions following the specified rules, such as '123+4', '96*15', '003-678', or '555/3'.

7. Press Enter to calculate and display the result.

8. If an invalid input format is entered, the program will guide you and display 'Invalid input format' or 'Overflow error' messages.

9. Continue using the calculator as needed.

## Notes

- Make sure to follow the provided rules and guidelines for input and operation.

- The program is designed to start at address $3100, and data starts at address $3000.

# Academic Integrity Statement:

Please note that all work included in this project is the original work of the author, and any external sources or references have been properly cited and credited. It is strictly prohibited to copy, reproduce, or use any part of this work without permission from the author.

If you choose to use any part of this work as a reference or resource, you are responsible for ensuring that you do not plagiarize or violate any academic integrity policies or guidelines. The author of this work cannot be held liable for any legal or academic consequences resulting from the misuse or misappropriation of this work.

Any unauthorized copying or use of this work may result in serious consequences, including but not limited to academic penalties, legal action, and damage to personal and professional reputation. Therefore, please use this work only as a reference and always ensure that you properly cite and attribute any sources or references used.
