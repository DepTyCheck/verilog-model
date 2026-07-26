-- Seed: 5531927965005270543,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (n : out std_logic_vector(3 downto 2));
end m;

architecture el of m is
  
begin
  -- Multi-driven assignments
  n <= n;
  n <= n;
  n <= "0Z";
  n <= n;
end el;

entity mjd is
  port (x : buffer integer; qigb : inout time);
end mjd;

architecture qij of mjd is
  
begin
  -- Single-driven assignments
  qigb <= 16#7.1212# ns;
  x <= x;
end qij;

library ieee;
use ieee.std_logic_1164.all;

entity yuu is
  port (y : inout boolean_vector(4 to 2); ef : buffer std_logic_vector(2 downto 4); jtsn : out std_logic_vector(0 to 1));
end yuu;

architecture lpswffyj of yuu is
  
begin
  lxtnqfnsjp : entity work.m
    port map (n => jtsn);
  yjgl : entity work.m
    port map (n => jtsn);
  
  -- Single-driven assignments
  y <= (others => TRUE);
  
  -- Multi-driven assignments
  jtsn <= jtsn;
  jtsn <= jtsn;
end lpswffyj;



-- Seed after: 3780568821423752884,7808623373429384027
