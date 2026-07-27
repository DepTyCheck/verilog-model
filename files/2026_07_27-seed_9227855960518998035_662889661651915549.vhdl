-- Seed: 9227855960518998035,662889661651915549

entity uvhwvzcyb is
  port (j : in real_vector(4 downto 3));
end uvhwvzcyb;

architecture tmvhdemx of uvhwvzcyb is
  
begin
  
end tmvhdemx;

library ieee;
use ieee.std_logic_1164.all;

entity exajhicm is
  port (wgyeshvhz : inout std_logic);
end exajhicm;

architecture xdr of exajhicm is
  signal gggqaimwo : real_vector(4 downto 3);
  signal vek : real_vector(4 downto 3);
  signal nqnk : real_vector(4 downto 3);
begin
  vqsd : entity work.uvhwvzcyb
    port map (j => nqnk);
  dqumjn : entity work.uvhwvzcyb
    port map (j => vek);
  cufltsqvqk : entity work.uvhwvzcyb
    port map (j => gggqaimwo);
  
  -- Single-driven assignments
  nqnk <= gggqaimwo;
  vek <= nqnk;
  
  -- Multi-driven assignments
  wgyeshvhz <= wgyeshvhz;
  wgyeshvhz <= '-';
end xdr;



-- Seed after: 15182999361942791513,662889661651915549
