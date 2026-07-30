-- Seed: 9131080728956613059,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity wdw is
  port (vjkk : inout time; b : out bit; fac : buffer std_logic_vector(1 downto 4));
end wdw;

architecture o of wdw is
  
begin
  -- Single-driven assignments
  vjkk <= 8#70645# ps;
  b <= b;
  
  -- Multi-driven assignments
  fac <= (others => '0');
end o;

entity bcrsnfkagj is
  port (my : inout time);
end bcrsnfkagj;

library ieee;
use ieee.std_logic_1164.all;

architecture wjggbt of bcrsnfkagj is
  signal owvzrxh : std_logic_vector(1 downto 4);
  signal vdjwhgkjtn : bit;
  signal jhevmmhea : time;
  signal b : std_logic_vector(1 downto 4);
  signal glqobd : bit;
  signal ji : time;
begin
  wut : entity work.wdw
    port map (vjkk => ji, b => glqobd, fac => b);
  zvxcl : entity work.wdw
    port map (vjkk => jhevmmhea, b => vdjwhgkjtn, fac => owvzrxh);
  
  -- Multi-driven assignments
  b <= b;
  b <= b;
  b <= b;
  b <= (others => '0');
end wjggbt;



-- Seed after: 6553991416732237436,4122021602305298647
