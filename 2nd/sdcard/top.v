`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date:    11:11:25 12/13/2012
// Design Name:
// Module Name:    top
// Project Name:
// Target Devices:
// Tool versions:
// Description:
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////
module top(
           input  CLK,
           input  XRST,
           output RS_TX,
           input  RS_RX,

           output SD_CS,
           input  SD_DI,
           output SD_SCLK,
           output SD_DO);

   wire MCLK, iclk;

   IBUFG ig (.I(CLK), .O(MCLK));
   BUFG bg (.I(MCLK), .O(iclk));

   wire rs_busy, sd_busy;
   reg rs_go, sd_read, sd_write;
   wire [7:0] data;
   wire [22:0] sd_addr;
   reg [8:0] sd_index;

   reg [2:0] state;
   reg [31:0] address;

   reg [7:0] write_data;
   reg write_enable;

   initial begin
      state <= 3'b0;
      address <= 32'h0;
   end

   assign sd_addr = 23'b0;
   // assign sd_index = 10'h1fe;

   // assign data = address[7:0];

   u232c #(.len(1)) u232c_inst
      (.clk(iclk),
       .data(data),
       .go(rs_go),
       .busy(rs_busy),
       .tx(RS_TX));

   wire [7:0] dummy_data;

   sd_cont sd_cont_inst
      (.clk(iclk),
       .sd_clk(SD_SCLK),
       .sd_ce(SD_CS),
       .sd_out(SD_DO),
       .sd_in(SD_DI),

       .sd_addr(sd_addr),
       .sd_read_data(data),
       .sd_write_data(write_data),
       .sd_write_enable(write_enable),
       .sd_index(sd_index),
       .sd_read(sd_read),
       .sd_write(sd_write),
       .sd_busy(sd_busy));

   always @ (posedge(iclk)) begin
      sd_read <= 0;
      write_enable <= 0;
      sd_write <= 0;
      rs_go <= 0;

      case (state)
        3'b000: begin
           sd_read <= 1;
           sd_index <= 9'b111111110;
           if (data == 8'h55) begin
              sd_index <= 9'b0;
              state <= 3'b001;
           end
        end

        3'b001: begin
           rs_go <= 1;
           state <= 3'b010;
        end

        3'b010: begin
           if (rs_go == 0 && rs_busy == 0) begin
              state <= 3'b011;
           end
        end

        3'b011: begin
           if (sd_index == 9'h1ff) begin
              state <= 3'b100;
           end else begin
              state <= 3'b001;
              sd_index <= sd_index + 9'b1;
           end
        end

        // Write to BRAM
        3'b100: begin
           write_enable <= 1;
           write_data <= 8'hff;
           sd_index <= 9'h0ff;
           state <= state + 1;
        end

        // Write back BRAM to SD
        3'b101: begin
           sd_write <= 1;
           state <= state + 1;
        end

        // Wait until write back ends
        3'b101: begin
           if (sd_write == 0 && sd_busy == 0)
             state <= state + 1;
        end

        // Reload BRAM
        3'b110: begin
           sd_read <= 1;
           state <= state + 1;
        end

        3'b111: begin
           if (sd_read == 0 && sd_busy == 0) begin
              sd_index <= 9'b0;
              state <= 3'b001;
           end
        end

        default:
          state <= 3'b0;
      endcase
   end

endmodule
