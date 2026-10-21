-- Seed: 16971201098815412231,17219977430298397867

entity abbar is
  port (ziz : out string(1 to 5); n : in real; qsovicoym : inout time);
end abbar;

architecture g of abbar is
  
begin
  -- Single-driven assignments
  ziz <= ziz;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity abd is
  port (aqnytj : inout std_logic; ekm : out real_vector(4 downto 3));
end abd;

architecture qw of abd is
  signal rde : time;
  signal jlkbvgfl : real;
  signal gabgxdryk : string(1 to 5);
  signal fzqcau : time;
  signal bkbwwrk : string(1 to 5);
  signal nau : time;
  signal vlgqpj : real;
  signal abvjvz : string(1 to 5);
begin
  ozmvnpm : entity work.abbar
    port map (ziz => abvjvz, n => vlgqpj, qsovicoym => nau);
  mv : entity work.abbar
    port map (ziz => bkbwwrk, n => vlgqpj, qsovicoym => fzqcau);
  wwcduacu : entity work.abbar
    port map (ziz => gabgxdryk, n => jlkbvgfl, qsovicoym => rde);
  
  -- Single-driven assignments
  jlkbvgfl <= vlgqpj;
  ekm <= (8#24623.4_6#, 2#1_0_1_0.0_0#);
  
  -- Multi-driven assignments
  aqnytj <= 'W';
  aqnytj <= 'H';
  aqnytj <= 'L';
  aqnytj <= '0';
end qw;



-- Seed after: 10051450303837375846,17219977430298397867
