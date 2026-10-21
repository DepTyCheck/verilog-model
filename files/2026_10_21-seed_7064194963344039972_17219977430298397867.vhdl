-- Seed: 7064194963344039972,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity ny is
  port (l : buffer std_logic; dkup : in time; eg : buffer integer);
end ny;

architecture yft of ny is
  
begin
  -- Single-driven assignments
  eg <= 441;
  
  -- Multi-driven assignments
  l <= '-';
  l <= 'X';
end yft;



-- Seed after: 10050326851132235118,17219977430298397867
