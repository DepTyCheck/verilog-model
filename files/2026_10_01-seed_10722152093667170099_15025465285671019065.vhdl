-- Seed: 10722152093667170099,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity rdxcsohmux is
  port (ofzn : buffer real; dgeg : out std_logic);
end rdxcsohmux;

architecture f of rdxcsohmux is
  
begin
  -- Single-driven assignments
  ofzn <= 8#2_3_3_7_2.76#;
end f;



-- Seed after: 18175219293604334391,15025465285671019065
