-- Seed: 9782217725154631260,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity eocisml is
  port (lhr : inout severity_level; op : out std_logic_vector(0 to 2));
end eocisml;

architecture r of eocisml is
  
begin
  -- Single-driven assignments
  lhr <= NOTE;
  
  -- Multi-driven assignments
  op <= op;
  op <= op;
end r;



-- Seed after: 17199171050143734140,12260394286515585877
