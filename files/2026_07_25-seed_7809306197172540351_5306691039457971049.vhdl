-- Seed: 7809306197172540351,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity xb is
  port (fvhypexp : out std_logic; rj : in time; xjbb : inout time);
end xb;

architecture azmck of xb is
  
begin
  -- Single-driven assignments
  xjbb <= 1 hr;
  
  -- Multi-driven assignments
  fvhypexp <= 'X';
  fvhypexp <= fvhypexp;
  fvhypexp <= fvhypexp;
end azmck;



-- Seed after: 4371543714659665053,5306691039457971049
