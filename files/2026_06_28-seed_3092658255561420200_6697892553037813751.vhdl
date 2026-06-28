-- Seed: 3092658255561420200,6697892553037813751

entity iigyytftyz is
  port (yxyvexapo : out bit_vector(1 to 0));
end iigyytftyz;

architecture hzexpgmtvc of iigyytftyz is
  
begin
  -- Single-driven assignments
  yxyvexapo <= (others => '0');
end hzexpgmtvc;

entity xpdgtgntxc is
  port (zjlluqn : in integer);
end xpdgtgntxc;

architecture kmcza of xpdgtgntxc is
  signal jkpyoaabq : bit_vector(1 to 0);
begin
  mzqdv : entity work.iigyytftyz
    port map (yxyvexapo => jkpyoaabq);
end kmcza;

library ieee;
use ieee.std_logic_1164.all;

entity hlf is
  port (jmoygi : in integer_vector(2 downto 4); egetkda : inout std_logic);
end hlf;

architecture mkslpur of hlf is
  signal huukj : bit_vector(1 to 0);
  signal l : bit_vector(1 to 0);
begin
  agu : entity work.iigyytftyz
    port map (yxyvexapo => l);
  n : entity work.iigyytftyz
    port map (yxyvexapo => huukj);
  
  -- Multi-driven assignments
  egetkda <= '0';
  egetkda <= 'L';
  egetkda <= 'Z';
end mkslpur;

entity zgephkvm is
  port (k : inout boolean_vector(3 downto 4));
end zgephkvm;

architecture tz of zgephkvm is
  
begin
  -- Single-driven assignments
  k <= (others => TRUE);
end tz;



-- Seed after: 18230582893496864361,6697892553037813751
