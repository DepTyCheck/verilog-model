-- Seed: 10759630064018014124,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity wynulntkg is
  port (qzpcgsdp : inout std_logic);
end wynulntkg;

architecture hyzxh of wynulntkg is
  
begin
  -- Multi-driven assignments
  qzpcgsdp <= '-';
  qzpcgsdp <= qzpcgsdp;
end hyzxh;

library ieee;
use ieee.std_logic_1164.all;

entity ti is
  port (rgp : inout std_logic; xgycw : in std_logic_vector(2 to 2); jyjicpejvj : buffer time; zp : out std_logic_vector(4 to 1));
end ti;

library ieee;
use ieee.std_logic_1164.all;

architecture xzizjdv of ti is
  signal scdhmi : std_logic;
  signal xtr : std_logic;
begin
  ybqvxqyazr : entity work.wynulntkg
    port map (qzpcgsdp => xtr);
  splqdhkrzb : entity work.wynulntkg
    port map (qzpcgsdp => rgp);
  zkjilcpee : entity work.wynulntkg
    port map (qzpcgsdp => scdhmi);
  ngrrpdlor : entity work.wynulntkg
    port map (qzpcgsdp => rgp);
  
  -- Single-driven assignments
  jyjicpejvj <= jyjicpejvj;
  
  -- Multi-driven assignments
  zp <= zp;
end xzizjdv;

entity gywnoj is
  port (wgx : inout time; imjkf : inout time; xvsilvdvgh : out integer_vector(2 downto 2); zemso : inout bit);
end gywnoj;

architecture wdx of gywnoj is
  
begin
  
end wdx;

entity ymzq is
  port (lw : buffer integer);
end ymzq;

library ieee;
use ieee.std_logic_1164.all;

architecture cpnn of ymzq is
  signal qopdj : bit;
  signal hbzc : integer_vector(2 downto 2);
  signal ptgscqvwgi : time;
  signal ldnojuidto : time;
  signal etqkicdcl : std_logic;
  signal mz : std_logic;
  signal viqbmsp : std_logic_vector(4 to 1);
  signal tphsexidsd : time;
  signal kfmgwc : std_logic_vector(2 to 2);
  signal ugtp : std_logic;
begin
  xoxxplphjj : entity work.ti
    port map (rgp => ugtp, xgycw => kfmgwc, jyjicpejvj => tphsexidsd, zp => viqbmsp);
  tewabkzwp : entity work.wynulntkg
    port map (qzpcgsdp => mz);
  zu : entity work.wynulntkg
    port map (qzpcgsdp => etqkicdcl);
  jlqtuzitz : entity work.gywnoj
    port map (wgx => ldnojuidto, imjkf => ptgscqvwgi, xvsilvdvgh => hbzc, zemso => qopdj);
  
  -- Single-driven assignments
  lw <= 2#1_1_0#;
  
  -- Multi-driven assignments
  etqkicdcl <= '1';
  ugtp <= 'H';
end cpnn;



-- Seed after: 7573842615593141182,7304262412290825129
