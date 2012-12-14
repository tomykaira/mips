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

   parameter WAIT_INITIALIZATION = 4'b0000;
   parameter SEND_READ_DATA      = 4'b0001;
   parameter WAIT_RS232C         = 4'b0010;
   parameter READ_NEXT_BYTE      = 4'b0011;
   parameter WRITE_TEST_BYTE     = 4'b0100;
   parameter WRITE_BACK_SD       = 4'b0101;
   parameter WAIT_WRITE_SD       = 4'b0110;
   parameter RELOAD_SD           = 4'b0111;
   parameter WAIT_READ_SD        = 4'b1000;
   parameter STOP                = 4'b1001;


   wire rs_busy, sd_busy;
   reg rs_go, sd_read, sd_write;
   wire [7:0] data;
   wire [22:0] sd_addr;
   reg [8:0] sd_index;

   reg [3:0] state, state_after_output;

   reg [7:0] write_data;
   reg write_enable;

   initial begin
      state <= 4'b0;
   end

   assign sd_addr = 23'b0;

   u232c #(.len(1)) u232c_inst
      (.clk(iclk),
       .data(data),
       .go(rs_go),
       .busy(rs_busy),
       .tx(RS_TX));

   wire [7:0] read_data;

   wire [9:0] debug;

   assign data = read_data;

   sd_cont sd_cont_inst
      (.clk(iclk),
       .sd_clk(SD_SCLK),
       .sd_ce(SD_CS),
       .sd_out(SD_DO),
       .sd_in(SD_DI),

       .sd_addr(sd_addr),
       .sd_read_data(read_data),
       .sd_write_data(write_data),
       .sd_write_enable(write_enable),
       .sd_index(sd_index),
       .sd_read(sd_read),
       .sd_write(sd_write),
       .sd_busy(sd_busy),

       .debug(debug));

   always @ (posedge(iclk)) begin
      sd_read <= 0;
      write_enable <= 0;
      sd_write <= 0;
      rs_go <= 0;

      case (state)
        WAIT_INITIALIZATION: begin
           sd_read <= 1;
           sd_index <= 9'b0;
           if (read_data != 0) begin
              sd_index <= 9'b0;
              state <= SEND_READ_DATA;
              state_after_output <= WRITE_TEST_BYTE;
           end
        end

        SEND_READ_DATA: begin
           rs_go <= 1;
           state <= WAIT_RS232C;
        end

        WAIT_RS232C: begin
           if (rs_busy == 0) begin
              state <= READ_NEXT_BYTE;
           end
        end

        READ_NEXT_BYTE: begin
           if (sd_index == 9'h1ff) begin
              state <= state_after_output;
           end else begin
              state <= SEND_READ_DATA;
              sd_index <= sd_index + 9'b1;
           end
        end

        // Write to BRAM
        WRITE_TEST_BYTE: begin
           write_enable <= 1;
           write_data <= 8'hff;
           sd_index <= 9'h0ff;
           state <= WRITE_BACK_SD;
        end

        // Write back BRAM to SD
        WRITE_BACK_SD: begin
           sd_write <= 1;
           state <= WAIT_WRITE_SD;
        end

        // Wait until write back ends
        WAIT_WRITE_SD: begin
           if (sd_write == 0 && sd_busy == 0)
             state <= RELOAD_SD;
        end

        // Reload BRAM
        RELOAD_SD: begin
           sd_read <= 1;
           state <= WAIT_READ_SD;
        end

        WAIT_READ_SD: begin
           if (sd_read == 0 && sd_busy == 0) begin
              sd_index <= 9'b0;
              state <= SEND_READ_DATA;
              state_after_output <= STOP;
           end
        end

        STOP:
           state <= STOP;

        default:
          state <= STOP;

      endcase
   end

endmodule
