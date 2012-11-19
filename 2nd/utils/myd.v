
module myd (C, D, Q);
   parameter
      WIDTH = 32;

   input C;
   input [WIDTH-1:0] D;
   output reg [WIDTH-1:0] Q;

   always @(posedge C)
     begin
        Q <= D;
     end
endmodule
