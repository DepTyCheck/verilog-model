-- Seed: 6922445568769912949,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity vcnhzpspvm is
  port (t : buffer std_logic; euprml : inout std_logic_vector(1 downto 1));
end vcnhzpspvm;

architecture ytg of vcnhzpspvm is
  
begin
  
end ytg;

library ieee;
use ieee.std_logic_1164.all;

entity csdltynp is
  port (lzrjdmli : inout std_logic_vector(0 downto 4));
end csdltynp;

library ieee;
use ieee.std_logic_1164.all;

architecture rvgktj of csdltynp is
  signal zgardx : std_logic_vector(1 downto 1);
  signal cxkswz : std_logic;
begin
  ctma : entity work.vcnhzpspvm
    port map (t => cxkswz, euprml => zgardx);
end rvgktj;

entity onukawyrg is
  port (jc : in real);
end onukawyrg;

library ieee;
use ieee.std_logic_1164.all;

architecture ga of onukawyrg is
  signal uhawikciv : std_logic_vector(1 downto 1);
  signal utpyjmzrpu : std_logic;
begin
  eamfybobqe : entity work.vcnhzpspvm
    port map (t => utpyjmzrpu, euprml => uhawikciv);
  hbgaox : entity work.vcnhzpspvm
    port map (t => utpyjmzrpu, euprml => uhawikciv);
  
  -- Multi-driven assignments
  uhawikciv <= uhawikciv;
  utpyjmzrpu <= utpyjmzrpu;
  uhawikciv <= uhawikciv;
  utpyjmzrpu <= utpyjmzrpu;
end ga;



-- Seed after: 9159661712494888660,1112937151005418631
