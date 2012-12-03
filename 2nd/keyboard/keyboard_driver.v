// Wrap keyboard driver, translate signals, add metadata

module keyboard_driver(input clk,
                       input        key_clk,
                       input        key_data,
                       output [7:0] key_status,
                       output [7:0] keycode);

   wire is_break;

   reg [7:0] recent_data[2:0];
   reg [7:0] data_buffer;
   reg [3:0] counter;
   reg [2:0] special_key_pressed;

   initial begin
      recent_data[2] <= 0;
      recent_data[1] <= 0;
      recent_data[0] <= 0;

      data_buffer <= 0;
      counter <= 0;

      special_key_pressed <= 0;
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
   reg changed;
   always @ (posedge(clk)) begin
      prev_clk[7:1] <= prev_clk[6:0];
      prev_clk[0] <= key_clk;
      changed <= 0;

      // negative edge of key_clk
      if (prev_clk == 8'b11110000) begin
         counter <= counter + 1;
         if ((counter >= 1 && counter <= 8)) begin
            data_buffer[6:0] <= data_buffer[7:1];
            data_buffer[7] <= key_data;
         end
      end

      if (counter == 11) begin
         counter <= 4'b0;
         recent_data[2] <= recent_data[1];
         recent_data[1] <= recent_data[0];
         recent_data[0] <= data_buffer;
         changed <= 1;
      end
   end

   reg [7:0] prev_code, next_code;
   reg       prev_break, next_break;
   reg       new_break_code;

   wire [2:0] is_special_key;
   keycode_resolver keycode_resolver_inst
     (.device_code(next_code),
      .shift_pressed(special_key_pressed[1]),
      .internal_code(keycode),
      .special_key(is_special_key));

   assign is_break = next_break;

   always @ (posedge (clk)) begin
      if (special_key_pressed[2] == 1'b1
          && is_special_key[2] == 1'b1
          && next_break == 1'b1)
        special_key_pressed[2] <= 1'b0;
      else if (special_key_pressed[2] == 1'b0
               && is_special_key[2] == 1'b1
               && next_break == 1'b0)
        special_key_pressed[2] <= 1'b1;


      if (special_key_pressed[1] == 1'b1
          && is_special_key[1] == 1'b1
          && next_break == 1'b1)
        special_key_pressed[1] <= 1'b0;
      else if (special_key_pressed[1] == 1'b0
               && is_special_key[1] == 1'b1
               && next_break == 1'b0)
        special_key_pressed[1] <= 1'b1;


      if (special_key_pressed[0] == 1'b1
          && is_special_key[0] == 1'b1
          && next_break == 1'b1)
        special_key_pressed[0] <= 1'b0;
      else if (special_key_pressed[0] == 1'b0
               && is_special_key[0] == 1'b1
               && next_break == 1'b0)
        special_key_pressed[0] <= 1'b1;
   end

   always @ (posedge(clk)) begin
      prev_code <= next_code;
      prev_break <= next_break;

      // special key or in the middle of data set
      if ((recent_data[2] == 8'hE0 && recent_data[1] == 8'hf0) || recent_data[1] == 8'hE0
          || recent_data[0] == 8'hF0 || recent_data[0] == 8'hE0) begin
         next_code <= prev_code;
         next_break <= prev_break;
         new_break_code <= 0;

         // break
      end else if (recent_data[1] == 8'hF0) begin
         next_code <= recent_data[0];
         next_break <= 1'b1;
         new_break_code <= changed;

         // make
      end else begin
         next_code <= recent_data[0];
         next_break <= 1'b0;
         new_break_code <= 0;
      end
   end

   assign key_status = {3'b0, special_key_pressed, new_break_code, is_break};
endmodule
