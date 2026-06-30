-- Seed: 12831680958755538329,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity lvkiz is
  port (qllnxw : buffer std_logic_vector(3 downto 4); pagjfij : inout time; qbza : inout bit_vector(1 to 0); eeorduj : buffer real);
end lvkiz;

architecture egoesvijwg of lvkiz is
  
begin
  -- Single-driven assignments
  pagjfij <= 16#0D80# ps;
  qbza <= (others => '0');
  eeorduj <= 4141.44020;
  
  -- Multi-driven assignments
  qllnxw <= (others => '0');
  qllnxw <= (others => '0');
  qllnxw <= (others => '0');
  qllnxw <= (others => '0');
end egoesvijwg;

entity jyvjb is
  port (efyzclm : buffer boolean_vector(2 to 3));
end jyvjb;

architecture xukm of jyvjb is
  
begin
  -- Single-driven assignments
  efyzclm <= (FALSE, TRUE);
end xukm;

library ieee;
use ieee.std_logic_1164.all;

entity kb is
  port (wcxecvkp : in integer; abatiik : linkage std_logic_vector(2 to 3));
end kb;

library ieee;
use ieee.std_logic_1164.all;

architecture xf of kb is
  signal oa : real;
  signal uihrfu : bit_vector(1 to 0);
  signal yrcvtsxz : time;
  signal ebs : std_logic_vector(3 downto 4);
  signal kzclk : boolean_vector(2 to 3);
  signal vtpzca : boolean_vector(2 to 3);
begin
  vkyhdxbpgj : entity work.jyvjb
    port map (efyzclm => vtpzca);
  yafs : entity work.jyvjb
    port map (efyzclm => kzclk);
  ut : entity work.lvkiz
    port map (qllnxw => ebs, pagjfij => yrcvtsxz, qbza => uihrfu, eeorduj => oa);
  
  -- Multi-driven assignments
  ebs <= (others => '0');
end xf;

entity jdapemzvv is
  port (kfu : linkage real; wuzdfaa : buffer integer);
end jdapemzvv;

library ieee;
use ieee.std_logic_1164.all;

architecture hdffrr of jdapemzvv is
  signal hx : boolean_vector(2 to 3);
  signal pmljh : real;
  signal em : bit_vector(1 to 0);
  signal hatjek : time;
  signal qn : std_logic_vector(3 downto 4);
begin
  kbdh : entity work.lvkiz
    port map (qllnxw => qn, pagjfij => hatjek, qbza => em, eeorduj => pmljh);
  xvf : entity work.jyvjb
    port map (efyzclm => hx);
  
  -- Single-driven assignments
  wuzdfaa <= 44;
  
  -- Multi-driven assignments
  qn <= (others => '0');
  qn <= (others => '0');
end hdffrr;



-- Seed after: 4493098651607030792,14629254427735353553
