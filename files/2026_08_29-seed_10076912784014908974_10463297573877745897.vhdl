-- Seed: 10076912784014908974,10463297573877745897

entity wth is
  port (s : out real; oybwdi : out integer_vector(3 to 0); z : in time);
end wth;

architecture mhzxztne of wth is
  
begin
  -- Single-driven assignments
  oybwdi <= (others => 0);
end mhzxztne;

library ieee;
use ieee.std_logic_1164.all;

entity ohlcell is
  port (yem : buffer std_logic_vector(0 to 2); gcff : out std_logic);
end ohlcell;

architecture deskxjmmcp of ohlcell is
  signal fth : integer_vector(3 to 0);
  signal haqim : real;
  signal q : time;
  signal vhdlrgtopg : integer_vector(3 to 0);
  signal vxjoe : real;
begin
  vyvgqiyfa : entity work.wth
    port map (s => vxjoe, oybwdi => vhdlrgtopg, z => q);
  qfb : entity work.wth
    port map (s => haqim, oybwdi => fth, z => q);
  
  -- Single-driven assignments
  q <= 113.3_4_4_2_4 ms;
  
  -- Multi-driven assignments
  yem <= yem;
  gcff <= gcff;
  yem <= ('Z', 'W', 'X');
end deskxjmmcp;



-- Seed after: 7669567696871893961,10463297573877745897
