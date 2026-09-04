-- Seed: 1686109872705560443,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity rraiqhgq is
  port (xoe : inout std_logic_vector(1 to 3));
end rraiqhgq;

architecture gzkjg of rraiqhgq is
  
begin
  -- Multi-driven assignments
  xoe <= "XXH";
  xoe <= xoe;
  xoe <= ('X', 'H', 'X');
end gzkjg;

entity yqf is
  port (hn : buffer time);
end yqf;

library ieee;
use ieee.std_logic_1164.all;

architecture dzoakdhul of yqf is
  signal chhxrmmobd : std_logic_vector(1 to 3);
begin
  yz : entity work.rraiqhgq
    port map (xoe => chhxrmmobd);
  sgomrn : entity work.rraiqhgq
    port map (xoe => chhxrmmobd);
  
  -- Single-driven assignments
  hn <= 16#7_2_D_4.EDB1E# ns;
  
  -- Multi-driven assignments
  chhxrmmobd <= chhxrmmobd;
  chhxrmmobd <= "X-0";
  chhxrmmobd <= ('H', '1', 'X');
end dzoakdhul;



-- Seed after: 17235733983443117447,4404421571376382767
