-- Seed: 15607730858955887989,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity tka is
  port (izlhq : buffer time; ckrmz : out time; vslk : inout std_logic);
end tka;

architecture xgzt of tka is
  
begin
  -- Single-driven assignments
  ckrmz <= 2#10.1_1_1# fs;
  izlhq <= ckrmz;
  
  -- Multi-driven assignments
  vslk <= '0';
  vslk <= '-';
  vslk <= 'L';
  vslk <= vslk;
end xgzt;

entity zbf is
  port (y : in integer; udn : in time_vector(1 to 0); gbvopuxcal : in time; ai : linkage integer);
end zbf;

architecture jtc of zbf is
  
begin
  
end jtc;



-- Seed after: 9965865805145265816,14141408946471626091
