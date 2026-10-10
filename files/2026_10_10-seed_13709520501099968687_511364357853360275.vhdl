-- Seed: 13709520501099968687,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity icnfpiivvj is
  port (rhrr : inout std_logic_vector(3 to 0));
end icnfpiivvj;

architecture vzul of icnfpiivvj is
  
begin
  -- Multi-driven assignments
  rhrr <= rhrr;
end vzul;

entity wur is
  port (cvmi : inout bit);
end wur;

library ieee;
use ieee.std_logic_1164.all;

architecture dhntlmf of wur is
  signal ifrjm : std_logic_vector(3 to 0);
begin
  vfcjmkdaq : entity work.icnfpiivvj
    port map (rhrr => ifrjm);
  qozyfoh : entity work.icnfpiivvj
    port map (rhrr => ifrjm);
  
  -- Single-driven assignments
  cvmi <= cvmi;
  
  -- Multi-driven assignments
  ifrjm <= (others => '0');
  ifrjm <= ifrjm;
  ifrjm <= ifrjm;
end dhntlmf;

library ieee;
use ieee.std_logic_1164.all;

entity ffjaiiwj is
  port (rfnpa : out time; f : linkage std_logic; imahe : buffer boolean);
end ffjaiiwj;

library ieee;
use ieee.std_logic_1164.all;

architecture avz of ffjaiiwj is
  signal gyeny : std_logic_vector(3 to 0);
  signal ch : bit;
begin
  zjdp : entity work.wur
    port map (cvmi => ch);
  ex : entity work.icnfpiivvj
    port map (rhrr => gyeny);
  urnpf : entity work.icnfpiivvj
    port map (rhrr => gyeny);
  kkshrbqoii : entity work.icnfpiivvj
    port map (rhrr => gyeny);
end avz;



-- Seed after: 11788706562725545858,511364357853360275
