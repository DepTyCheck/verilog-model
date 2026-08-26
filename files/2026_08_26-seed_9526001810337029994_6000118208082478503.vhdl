-- Seed: 9526001810337029994,6000118208082478503

entity cgtjwm is
  port (qpw : inout bit; qsh : in integer; gjbw : out time_vector(2 to 4));
end cgtjwm;

architecture rqgfil of cgtjwm is
  
begin
  -- Single-driven assignments
  gjbw <= (140 ms, 2 sec, 16#4_A_F_B# ms);
  qpw <= qpw;
end rqgfil;



-- Seed after: 9043433824353686627,6000118208082478503
