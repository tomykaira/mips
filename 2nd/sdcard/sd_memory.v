module sd_memory(input clk,
                 input [8:0]      addra,
                 input [7:0]      dina,
                 input            wea,

                 input [8:0]      addrb,
                 output reg [7:0] doutb);

   reg [7:0] RAM[511:0];

   integer i;
   initial begin
      for(i=0;i<512;i=i+1)
        RAM[i]=0;
   end

   always @ (posedge(clk)) begin
      if (wea == 1'b1)
         RAM[addra] <= dina;

      doutb <= RAM[addrb];
   end
endmodule
