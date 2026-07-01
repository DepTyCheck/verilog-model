-- Seed: 5513074963426166765,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity dplnnd is
  port (gcwwhtddg : in time; yj : in std_logic_vector(1 downto 0));
end dplnnd;

architecture vdwhze of dplnnd is
  
begin
  
end vdwhze;

library ieee;
use ieee.std_logic_1164.all;

entity ukkbad is
  port (so : buffer std_logic_vector(0 downto 0); al : inout time; qghz : in std_logic; jiuj : linkage std_logic);
end ukkbad;

architecture buzbvwdezt of ukkbad is
  
begin
  
end buzbvwdezt;

library ieee;
use ieee.std_logic_1164.all;

entity sm is
  port (a : out std_logic_vector(1 downto 3); apqptkq : inout integer; uwkfnmnen : out time; dagfep : linkage time);
end sm;

library ieee;
use ieee.std_logic_1164.all;

architecture kfk of sm is
  signal vam : std_logic;
  signal pdbujvqdom : std_logic_vector(0 downto 0);
begin
  veqdhvdu : entity work.ukkbad
    port map (so => pdbujvqdom, al => uwkfnmnen, qghz => vam, jiuj => vam);
  
  -- Single-driven assignments
  apqptkq <= 2#011#;
  
  -- Multi-driven assignments
  a <= "";
  a <= (others => '0');
end kfk;

entity tdxpldako is
  port (owhboidm : inout bit_vector(2 downto 2));
end tdxpldako;

library ieee;
use ieee.std_logic_1164.all;

architecture t of tdxpldako is
  signal atjhdvaz : time;
  signal wpwkq : integer;
  signal llxeaog : std_logic_vector(1 downto 3);
  signal atsdmea : std_logic_vector(1 downto 0);
  signal zxaeuy : time;
begin
  jrenmh : entity work.dplnnd
    port map (gcwwhtddg => zxaeuy, yj => atsdmea);
  re : entity work.sm
    port map (a => llxeaog, apqptkq => wpwkq, uwkfnmnen => atjhdvaz, dagfep => zxaeuy);
end t;



-- Seed after: 7891543029588461764,6882842853887419669
