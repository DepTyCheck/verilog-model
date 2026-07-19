-- Seed: 4416947668217862416,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity ave is
  port (pxamq : inout time; ejdjdtlkg : linkage std_logic_vector(0 downto 4); qit : out std_logic_vector(2 to 3));
end ave;

architecture tssygfr of ave is
  
begin
  
end tssygfr;

entity ffiksar is
  port (ris : buffer integer);
end ffiksar;

library ieee;
use ieee.std_logic_1164.all;

architecture imuimocov of ffiksar is
  signal xuaur : std_logic_vector(0 downto 4);
  signal z : time;
  signal qyaul : std_logic_vector(2 to 3);
  signal btivbqhlnd : time;
  signal loz : std_logic_vector(0 downto 4);
  signal owq : time;
  signal rvfksn : std_logic_vector(2 to 3);
  signal kd : std_logic_vector(0 downto 4);
  signal qpnorpswmf : time;
begin
  wstmqmz : entity work.ave
    port map (pxamq => qpnorpswmf, ejdjdtlkg => kd, qit => rvfksn);
  esk : entity work.ave
    port map (pxamq => owq, ejdjdtlkg => loz, qit => rvfksn);
  onzxh : entity work.ave
    port map (pxamq => btivbqhlnd, ejdjdtlkg => kd, qit => qyaul);
  fhca : entity work.ave
    port map (pxamq => z, ejdjdtlkg => xuaur, qit => rvfksn);
end imuimocov;

entity ckjjnnyral is
  port (ocmkkzo : inout time_vector(2 to 2));
end ckjjnnyral;

library ieee;
use ieee.std_logic_1164.all;

architecture fc of ckjjnnyral is
  signal tzjhcddx : std_logic_vector(2 to 3);
  signal idzl : std_logic_vector(0 downto 4);
  signal eewoiafyt : time;
begin
  kyraxbk : entity work.ave
    port map (pxamq => eewoiafyt, ejdjdtlkg => idzl, qit => tzjhcddx);
  
  -- Single-driven assignments
  ocmkkzo <= ocmkkzo;
  
  -- Multi-driven assignments
  idzl <= "";
  tzjhcddx <= tzjhcddx;
end fc;



-- Seed after: 2656664929198058369,5511103086789671269
