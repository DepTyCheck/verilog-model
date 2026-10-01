-- Seed: 4727205752488818338,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity jznnppdm is
  port (h : in boolean_vector(2 to 4); t : linkage std_logic);
end jznnppdm;

architecture vypdhkjs of jznnppdm is
  
begin
  
end vypdhkjs;

library ieee;
use ieee.std_logic_1164.all;

entity awdag is
  port (ynlqzc : buffer std_logic_vector(4 to 1); ejh : inout std_logic_vector(0 downto 4));
end awdag;

library ieee;
use ieee.std_logic_1164.all;

architecture pgij of awdag is
  signal q : std_logic;
  signal jiflycepri : std_logic;
  signal tluwp : boolean_vector(2 to 4);
  signal lljlckdjzg : std_logic;
  signal dlpguupayw : boolean_vector(2 to 4);
begin
  vdo : entity work.jznnppdm
    port map (h => dlpguupayw, t => lljlckdjzg);
  zjll : entity work.jznnppdm
    port map (h => tluwp, t => jiflycepri);
  rfgsd : entity work.jznnppdm
    port map (h => dlpguupayw, t => q);
  
  -- Single-driven assignments
  tluwp <= dlpguupayw;
  dlpguupayw <= dlpguupayw;
  
  -- Multi-driven assignments
  ejh <= "";
  ejh <= "";
  ynlqzc <= "";
end pgij;

entity ifew is
  port (h : buffer time; knry : inout real);
end ifew;

library ieee;
use ieee.std_logic_1164.all;

architecture b of ifew is
  signal rkqqhfuwfr : std_logic_vector(0 downto 4);
  signal ydcxepazo : std_logic_vector(4 to 1);
  signal hovfkq : std_logic;
  signal csz : boolean_vector(2 to 4);
begin
  ov : entity work.jznnppdm
    port map (h => csz, t => hovfkq);
  sjupeqn : entity work.awdag
    port map (ynlqzc => ydcxepazo, ejh => rkqqhfuwfr);
  
  -- Single-driven assignments
  h <= 2#0010.1_1_0_1_0# fs;
  knry <= knry;
  
  -- Multi-driven assignments
  hovfkq <= 'Z';
end b;



-- Seed after: 14413785587964439484,15025465285671019065
