-- Seed: 1210684169311063463,511364357853360275

entity wnqp is
  port (qy : out time; ttrgucq : out time);
end wnqp;

architecture w of wnqp is
  
begin
  -- Single-driven assignments
  ttrgucq <= 1 min;
  qy <= ttrgucq;
end w;

library ieee;
use ieee.std_logic_1164.all;

entity vaywsdqks is
  port (xzlmfs : in std_logic_vector(4 downto 1); dbcvpizt : inout bit_vector(4 downto 2); wykcerx : buffer time);
end vaywsdqks;

architecture fdpxrqg of vaywsdqks is
  signal lmjp : time;
  signal fmb : time;
  signal fymoeasbja : time;
begin
  gilixej : entity work.wnqp
    port map (qy => fymoeasbja, ttrgucq => fmb);
  qvdscurx : entity work.wnqp
    port map (qy => lmjp, ttrgucq => wykcerx);
  
  -- Single-driven assignments
  dbcvpizt <= ('0', '1', '1');
end fdpxrqg;

library ieee;
use ieee.std_logic_1164.all;

entity wozjzqskf is
  port (ht : linkage character; olunh : out string(4 to 3); zlumhin : inout std_logic_vector(1 to 4); cpvbyl : inout time);
end wozjzqskf;

architecture gldqcnzhea of wozjzqskf is
  signal hajofwn : time;
  signal bfztaam : time;
  signal gzypbo : time;
begin
  pmlieqwa : entity work.wnqp
    port map (qy => gzypbo, ttrgucq => cpvbyl);
  nyjuoq : entity work.wnqp
    port map (qy => bfztaam, ttrgucq => hajofwn);
  
  -- Single-driven assignments
  olunh <= olunh;
  
  -- Multi-driven assignments
  zlumhin <= zlumhin;
end gldqcnzhea;



-- Seed after: 2615450888294601323,511364357853360275
