module sd_cache(input clk,

                input         read,
                input         write,
                input         busy,

                output reg    read_spi,
                output reg    write_spi,
                output reg    write_ram,

                input [31:0]  addr,
                output [22:0] block,

                output reg    ready);

   reg [22:0] current_block, reading_block;
   wire [22:0] required_block;
   reg prev_busy;
   reg [3:0] state;
   reg dirty;

   parameter WAIT        = 4'h0;
   parameter WRITE_READY = 4'h1;
   parameter READ_READY  = 4'h2;
   parameter WRITE_WAIT  = 4'h4;
   parameter READ_WAIT   = 4'h7;

   assign required_block = addr[31:9];
   assign block          = read_spi == 1'b1 ? reading_block : current_block;

   initial begin
      current_block <= 22'b1111111111111111111111;
      reading_block <= 22'b1111111111111111111111;
      prev_busy     <= 1'b0;
      ready         <= 1'b0;
      dirty         <= 1'b0;
      state         <= WAIT;
   end

   always @ (posedge(clk)) begin
      prev_busy <= busy;
      read_spi  <= 1'b0;
      write_spi <= 1'b0;
      write_ram <= 1'b0;
      ready     <= 1'b0;

      case (state)
        WAIT:
           if (read == 1 || write == 1)

             if (required_block == current_block) begin

                if (read == 1)
                  state <= READ_READY;

                else if (write == 1)
                  state <= WRITE_READY;

             // start write
             end else if (dirty == 1) begin
                write_spi <= 1'b1;
                state <= WRITE_WAIT;

                // start read
             end else begin
                read_spi      <= 1'b1;
                reading_block <= required_block;

                state <= READ_WAIT;
             end

        WRITE_READY: begin
           dirty     <= 1'b1;
           write_ram <= 1'b1;
           ready     <= 1;

           state <= WAIT;
        end

        READ_READY: begin
           ready     <= 1;

           state <= WAIT;
        end

        WRITE_WAIT: begin
           if (write_spi == 0 && busy == 0) begin
              dirty <= 0;
              state <= WAIT;
           end
        end

        READ_WAIT: begin
           if (read_spi == 0 && busy == 0) begin
              state         <= WAIT;
              current_block <= reading_block;
           end
        end

      endcase
   end

endmodule
