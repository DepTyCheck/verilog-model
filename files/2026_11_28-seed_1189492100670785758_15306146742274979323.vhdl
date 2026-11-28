-- Seed: 1189492100670785758,15306146742274979323

entity hkc is
  port (izgadde : out boolean_vector(1 downto 4); jlqociwy : in time);
end hkc;

architecture isldgd of hkc is
  
begin
  -- Single-driven assignments
  izgadde <= (others => TRUE);
end isldgd;

library ieee;
use ieee.std_logic_1164.all;

entity ki is
  port (pjxdky : out time; dfs : linkage std_logic; kxznl : buffer integer_vector(4 downto 3); ladvybfjza : in boolean);
end ki;

architecture gyayh of ki is
  signal ydt : time;
  signal wyipciaon : boolean_vector(1 downto 4);
  signal jln : time;
  signal ubeivwf : boolean_vector(1 downto 4);
begin
  bss : entity work.hkc
    port map (izgadde => ubeivwf, jlqociwy => jln);
  qecvoaj : entity work.hkc
    port map (izgadde => wyipciaon, jlqociwy => ydt);
end gyayh;

entity ut is
  port (dqacvisnqw : buffer boolean_vector(0 downto 1));
end ut;

library ieee;
use ieee.std_logic_1164.all;

architecture avvroexc of ut is
  signal uiu : boolean;
  signal vcaixz : integer_vector(4 downto 3);
  signal rqzq : std_logic;
  signal ptslcpkmqj : time;
  signal whsufzcfg : time;
  signal gwx : boolean_vector(1 downto 4);
  signal jjdoaf : time;
  signal ylpfnnorjd : boolean_vector(1 downto 4);
begin
  kc : entity work.hkc
    port map (izgadde => ylpfnnorjd, jlqociwy => jjdoaf);
  ucyvznhlvx : entity work.hkc
    port map (izgadde => gwx, jlqociwy => whsufzcfg);
  aeck : entity work.ki
    port map (pjxdky => ptslcpkmqj, dfs => rqzq, kxznl => vcaixz, ladvybfjza => uiu);
  htliqmw : entity work.hkc
    port map (izgadde => dqacvisnqw, jlqociwy => ptslcpkmqj);
  
  -- Multi-driven assignments
  rqzq <= 'H';
end avvroexc;



-- Seed after: 15823447658130647853,15306146742274979323
