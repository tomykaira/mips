// Simple keyboard driver
// It does not handle special keys, starts with E0

// input
//   clk : system clock
//   key_clk : clock sent from keyboard
//   key_data : data from keyboard
// output :
//   keycode : read key code
//   is_break : 1, if the code is break code, 0 if it is make code
module keyboard_driver(input clk,
                       input        key_clk,
                       input        key_data,
                       output [7:0] keycode,
                       output       is_break);

   reg [7:0] recent_data[2:0];
   reg [7:0] data_buffer;
   reg [3:0] counter;

   initial begin
      recent_data[2] <= 0;
      recent_data[1] <= 0;
      recent_data[0] <= 0;

      data_buffer <= 0;
      counter <= 0;
   end

   // 0: no data
   // 1: start of first byte received
   // 2: first bit of first byte
   // 3: 2nd
   // 4: 3rd
   // 5
   // 6
   // 7
   // 8
   // 9: 8th
   // 10: parity
   // 11: end bit
   reg [7:0] prev_clk;
   always @ (posedge(clk)) begin
      prev_clk[7:1] <= prev_clk[6:0];
      prev_clk[0] <= key_clk;

      // positive edge of key_clk
      if (prev_clk == 8'b11110000) begin
         // if (counter == 0)
         //   if (key_data == 0)
         //     counter <= 4'b1;
         //   else
         //     counter <= 4'b0;
         // else begin
            counter <= counter + 1;
            if ((counter >= 1 && counter <= 8)) begin
               data_buffer[6:0] <= data_buffer[7:1];
               data_buffer[7] <= key_data;
            end
         // end
      end

      if (counter == 11) begin
         counter <= 4'b0;
         recent_data[2] <= recent_data[1];
         recent_data[1] <= recent_data[0];
         recent_data[0] <= data_buffer;
      end
   end

   reg [7:0] prev_code, next_code;
   reg prev_break, next_break;

   assign keycode = next_code;
   assign is_break = next_break;

   always @ (posedge(clk)) begin
      prev_code <= next_code;
      prev_break <= next_break;

      // special key or in the middle of data set
      if ((recent_data[2] == 8'hE0 && recent_data[1] == 8'hf0) || recent_data[1] == 8'hE0
           || recent_data[0] == 8'hF0 || recent_data[0] == 8'hE0) begin
         next_code <= prev_code;
         next_break <= prev_break;

      // break
      end else if (recent_data[1] == 8'hF0) begin
         next_code <= recent_data[0];
         next_break <= 1'b1;

      // make
      end else begin
         next_code <= recent_data[0];
         next_break <= 1'b0;
      end
   end
endmodule
