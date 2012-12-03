/*
 * Map device code to internal code.
 * basically mapping to ascii code. for detail, see keycode.rb
 * special_keys are ctrl, shift, alt in this order.
 * See: http://www.computer-engineering.org/ps2keyboard/scancodes2.html
*/
module keycode_resolver (input [7:0] device_code,
                         output [7:0] internal_code,
                         output [2:0] special_key);
   reg [6:0] CODE_MAP [127:0];

   initial
      $readmemb("keycode.dat", CODE_MAP);

   assign internal_code = {1'b0, CODE_MAP[device_code[6:0]]};

   wire control, shift, alt;
   assign alt     = device_code == 8'h11 ? 1'b1 : 1'b0;
   assign shift   = device_code == 8'h12 ? 1'b1 : 1'b0;
   assign control = device_code == 8'h14 || device_code == 7'h88
                    ? 1'b1 : 1'b0;

   assign special_key = {control, shift, alt};

endmodule
