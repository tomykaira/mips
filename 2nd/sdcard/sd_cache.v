module sd_cache(input clk,

                input        sd_go,
                input [31:0] sd_addr,
                input        sd_busy,
                output reg   to_load,
                output reg   ready);

   reg [22:0] current_block, loading_block;
   wire [22:0] required_block;
   reg prev_busy;

   assign required_block =  sd_addr[31:9];

   initial begin
      current_block <= 22'b1111111111111111111111;
      loading_block <= 22'b1111111111111111111111;
      prev_busy <= 1'b0;
      ready <= 1'b0;
   end

   always @ (posedge(clk)) begin
      prev_busy <= sd_busy;
      to_load   <= 1'b0;
      ready     <= 1'b0;

      if (sd_go == 1'b1) begin
         if (required_block == current_block) begin
            ready <= 1'b1;
            to_load <= 1'b0;
         end else if (sd_busy == 1'b0) begin
            if (prev_busy == 1'b1) begin
               current_block <= loading_block;
            end else begin
               loading_block <= required_block;
               to_load <= 1'b1;
            end
         end
      end
   end

endmodule
