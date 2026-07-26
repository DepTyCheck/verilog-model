-- Seed: 12671013896455603233,7808623373429384027

entity wudid is
  port (i : buffer boolean; hdfosb : inout boolean);
end wudid;

architecture w of wudid is
  
begin
  -- Single-driven assignments
  hdfosb <= TRUE;
end w;

library ieee;
use ieee.std_logic_1164.all;

entity et is
  port (wd : linkage real; abthedslgm : inout std_logic);
end et;

architecture cx of et is
  
begin
  -- Multi-driven assignments
  abthedslgm <= 'Z';
  abthedslgm <= '1';
  abthedslgm <= '1';
  abthedslgm <= '1';
end cx;



-- Seed after: 2515449633603751273,7808623373429384027
