module sd_memory(input clk,
                 input [8:0]      addra,
                 input [7:0]      dina,
                 input            wea,

                 input [8:0]      addrb,
                 output reg [7:0] doutb);

   reg [7:0] RAM[511:0];

   always @ (posedge(clk)) begin
      if (wea == 1'b1)
         RAM[addra] <= dina;

      doutb <= RAM[addrb];
   end
endmodule
