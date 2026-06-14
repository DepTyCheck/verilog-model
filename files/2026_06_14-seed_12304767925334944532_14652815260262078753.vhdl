-- Seed: 12304767925334944532,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity pwuv is
  port (mex : in time; y : inout std_logic; s : in boolean);
end pwuv;

architecture dwfm of pwuv is
  
begin
  -- Multi-driven assignments
  y <= 'Z';
  y <= 'W';
  y <= 'Z';
end dwfm;



-- Seed after: 15389847118132299023,14652815260262078753
