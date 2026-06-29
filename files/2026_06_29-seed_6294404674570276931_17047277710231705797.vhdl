-- Seed: 6294404674570276931,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity be is
  port (s : in std_logic; zunhn : inout time);
end be;

architecture filp of be is
  
begin
  -- Single-driven assignments
  zunhn <= 3 min;
end filp;

entity v is
  port (bjzr : in real; tziya : inout boolean_vector(1 downto 4); hbiwvaccro : in real; rv : buffer time);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture h of v is
  signal nlpue : time;
  signal clppnuo : time;
  signal zfxq : std_logic;
begin
  mqjmogvn : entity work.be
    port map (s => zfxq, zunhn => clppnuo);
  rja : entity work.be
    port map (s => zfxq, zunhn => nlpue);
  jzpidamij : entity work.be
    port map (s => zfxq, zunhn => rv);
  
  -- Single-driven assignments
  tziya <= (others => TRUE);
end h;

library ieee;
use ieee.std_logic_1164.all;

entity ma is
  port (gh : out std_logic_vector(3 to 3));
end ma;

library ieee;
use ieee.std_logic_1164.all;

architecture bnej of ma is
  signal nwqm : time;
  signal vs : boolean_vector(1 downto 4);
  signal abf : real;
  signal wmyb : time;
  signal vh : real;
  signal dh : boolean_vector(1 downto 4);
  signal tl : real;
  signal nwucysp : time;
  signal ryfbftexd : std_logic;
  signal hboejoxeyg : time;
  signal ez : std_logic;
begin
  oyv : entity work.be
    port map (s => ez, zunhn => hboejoxeyg);
  za : entity work.be
    port map (s => ryfbftexd, zunhn => nwucysp);
  c : entity work.v
    port map (bjzr => tl, tziya => dh, hbiwvaccro => vh, rv => wmyb);
  bvoypckl : entity work.v
    port map (bjzr => abf, tziya => vs, hbiwvaccro => tl, rv => nwqm);
  
  -- Single-driven assignments
  tl <= 8#2_3_2_3.035#;
  abf <= 16#B0C3C.31#;
  
  -- Multi-driven assignments
  ez <= '0';
  gh <= "X";
  gh <= "W";
end bnej;



-- Seed after: 11694796447671539599,17047277710231705797
