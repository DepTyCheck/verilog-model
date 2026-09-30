-- Seed: 12498451620611475836,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity mowg is
  port (nehuugd : in std_logic_vector(2 to 2); ptz : inout time; bsccljd : buffer std_logic_vector(2 to 0); vzj : buffer std_logic_vector(3 downto 0));
end mowg;

architecture akm of mowg is
  
begin
  -- Single-driven assignments
  ptz <= 2330.3 ns;
  
  -- Multi-driven assignments
  vzj <= "WW-Z";
end akm;

entity dvkkwzz is
  port (lxlhcuyi : linkage string(1 to 5); idm : inout integer_vector(4 downto 2); x : in integer_vector(2 to 0); tqj : buffer time);
end dvkkwzz;

architecture kbhn of dvkkwzz is
  
begin
  -- Single-driven assignments
  tqj <= 1 ms;
  idm <= (16#5#, 0211, 8#5#);
end kbhn;

library ieee;
use ieee.std_logic_1164.all;

entity aasfgld is
  port (dyhe : inout std_logic; rdsv : linkage std_logic);
end aasfgld;

library ieee;
use ieee.std_logic_1164.all;

architecture by of aasfgld is
  signal jwepecwuve : std_logic_vector(2 to 0);
  signal a : time;
  signal rczzn : std_logic_vector(3 downto 0);
  signal onohnyj : time;
  signal p : std_logic_vector(3 downto 0);
  signal lbpndjirhs : std_logic_vector(2 to 0);
  signal t : time;
  signal udijy : std_logic_vector(2 to 2);
begin
  bvl : entity work.mowg
    port map (nehuugd => udijy, ptz => t, bsccljd => lbpndjirhs, vzj => p);
  ktghyqb : entity work.mowg
    port map (nehuugd => udijy, ptz => onohnyj, bsccljd => lbpndjirhs, vzj => rczzn);
  usljxjofg : entity work.mowg
    port map (nehuugd => udijy, ptz => a, bsccljd => jwepecwuve, vzj => rczzn);
  
  -- Multi-driven assignments
  dyhe <= dyhe;
  dyhe <= dyhe;
  dyhe <= dyhe;
  dyhe <= 'U';
end by;



-- Seed after: 8613208850060650358,12260394286515585877
