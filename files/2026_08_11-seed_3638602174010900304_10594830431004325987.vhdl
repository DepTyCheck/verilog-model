-- Seed: 3638602174010900304,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity lvfq is
  port (hucjvwqig : out std_logic_vector(2 to 2); vr : buffer bit);
end lvfq;

architecture m of lvfq is
  
begin
  -- Single-driven assignments
  vr <= vr;
  
  -- Multi-driven assignments
  hucjvwqig <= (others => 'Z');
end m;

entity hisw is
  port (eaiwka : out integer; npjxknxmvy : in real; kzddmo : out integer);
end hisw;

library ieee;
use ieee.std_logic_1164.all;

architecture uc of hisw is
  signal c : bit;
  signal y : bit;
  signal i : bit;
  signal wzek : std_logic_vector(2 to 2);
begin
  hgthb : entity work.lvfq
    port map (hucjvwqig => wzek, vr => i);
  f : entity work.lvfq
    port map (hucjvwqig => wzek, vr => y);
  vzdutjx : entity work.lvfq
    port map (hucjvwqig => wzek, vr => c);
  
  -- Single-driven assignments
  kzddmo <= kzddmo;
  
  -- Multi-driven assignments
  wzek <= "1";
  wzek <= (others => 'H');
end uc;



-- Seed after: 5134218614630030419,10594830431004325987
