-- Seed: 3998671325597340738,3687118713772291287

entity uxphxngol is
  port (npsnm : in real; ljcq : buffer integer; usatwno : linkage integer_vector(4 to 4));
end uxphxngol;

architecture rghvyh of uxphxngol is
  
begin
  
end rghvyh;

entity estflha is
  port (jqfna : buffer integer; y : inout integer; qaow : inout real);
end estflha;

architecture aubily of estflha is
  signal pxk : integer_vector(4 to 4);
  signal q : integer;
  signal bbjt : real;
  signal ls : integer_vector(4 to 4);
  signal ludf : integer;
begin
  saa : entity work.uxphxngol
    port map (npsnm => qaow, ljcq => ludf, usatwno => ls);
  uky : entity work.uxphxngol
    port map (npsnm => bbjt, ljcq => q, usatwno => pxk);
  
  -- Single-driven assignments
  qaow <= 3_4_1_2.0_4;
end aubily;



-- Seed after: 7377329626576634890,3687118713772291287
