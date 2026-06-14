-- Seed: 10111847110703603264,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity jlt is
  port (pwzyvedtx : inout std_logic; un : linkage integer_vector(4 downto 1); usyapbk : inout time);
end jlt;

architecture xj of jlt is
  
begin
  -- Single-driven assignments
  usyapbk <= 430.1 us;
  
  -- Multi-driven assignments
  pwzyvedtx <= '1';
  pwzyvedtx <= 'W';
  pwzyvedtx <= '0';
end xj;



-- Seed after: 17984129208856264759,14652815260262078753
