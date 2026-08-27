-- Seed: 16969429557264130632,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity dordtvna is
  port (hesgwt : in severity_level; ecck : out bit_vector(1 downto 0); wevxzg : buffer std_logic; qeksjtlyt : buffer real);
end dordtvna;

architecture ncmwef of dordtvna is
  
begin
  -- Single-driven assignments
  qeksjtlyt <= 4_1_3_4_4.12;
  ecck <= ('0', '1');
  
  -- Multi-driven assignments
  wevxzg <= '-';
  wevxzg <= 'W';
  wevxzg <= wevxzg;
end ncmwef;

library ieee;
use ieee.std_logic_1164.all;

entity mkxsgfhyi is
  port (nyssp : buffer std_logic_vector(0 downto 0); nwc : out integer; vcwqgorysz : linkage boolean_vector(1 downto 2));
end mkxsgfhyi;

library ieee;
use ieee.std_logic_1164.all;

architecture d of mkxsgfhyi is
  signal rkqepsvz : real;
  signal dwc : bit_vector(1 downto 0);
  signal mwbf : real;
  signal ekpaachafh : std_logic;
  signal vyiqmic : bit_vector(1 downto 0);
  signal ew : severity_level;
begin
  abrtrzkv : entity work.dordtvna
    port map (hesgwt => ew, ecck => vyiqmic, wevxzg => ekpaachafh, qeksjtlyt => mwbf);
  zqjsx : entity work.dordtvna
    port map (hesgwt => ew, ecck => dwc, wevxzg => ekpaachafh, qeksjtlyt => rkqepsvz);
  
  -- Multi-driven assignments
  nyssp <= (others => '1');
  nyssp <= (others => 'W');
  nyssp <= "L";
end d;



-- Seed after: 16598176026099906896,6299883410057943775
