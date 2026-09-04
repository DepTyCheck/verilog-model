-- Seed: 11678387982976915842,4404421571376382767

entity fyrb is
  port (qz : out real; cb : buffer real);
end fyrb;

architecture xfenmuehlt of fyrb is
  
begin
  -- Single-driven assignments
  cb <= cb;
  qz <= 30.1430;
end xfenmuehlt;

entity qcjjzd is
  port (yi : in time_vector(4 downto 4); mhwwtfnki : out time_vector(2 downto 2));
end qcjjzd;

architecture prqxzoghs of qcjjzd is
  signal fwshzrjzy : real;
  signal kyvvjms : real;
begin
  gkaxguc : entity work.fyrb
    port map (qz => kyvvjms, cb => fwshzrjzy);
  
  -- Single-driven assignments
  mhwwtfnki <= mhwwtfnki;
end prqxzoghs;

library ieee;
use ieee.std_logic_1164.all;

entity wvw is
  port (y : buffer std_logic; lzr : inout real; yqtxykkl : inout bit);
end wvw;

architecture uyimbiz of wvw is
  signal awn : real;
  signal vsozrr : time_vector(2 downto 2);
  signal rpibhoux : time_vector(4 downto 4);
begin
  hyg : entity work.qcjjzd
    port map (yi => rpibhoux, mhwwtfnki => vsozrr);
  tm : entity work.fyrb
    port map (qz => awn, cb => lzr);
  
  -- Single-driven assignments
  yqtxykkl <= '1';
  rpibhoux <= rpibhoux;
  
  -- Multi-driven assignments
  y <= '-';
  y <= y;
  y <= y;
  y <= 'X';
end uyimbiz;



-- Seed after: 5070086873075704526,4404421571376382767
