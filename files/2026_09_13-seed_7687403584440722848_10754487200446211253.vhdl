-- Seed: 7687403584440722848,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity wvtshb is
  port (p : inout real; qd : out real; za : buffer std_logic; osigi : inout time);
end wvtshb;

architecture bj of wvtshb is
  
begin
  -- Single-driven assignments
  osigi <= 0 hr;
  p <= 3110.2321;
  qd <= qd;
  
  -- Multi-driven assignments
  za <= 'U';
  za <= za;
  za <= 'W';
  za <= 'Z';
end bj;



-- Seed after: 14781998831774534386,10754487200446211253
