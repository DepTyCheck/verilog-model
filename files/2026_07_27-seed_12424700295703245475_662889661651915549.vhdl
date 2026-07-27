-- Seed: 12424700295703245475,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity ontutmglde is
  port (qgarxanawa : buffer string(3 downto 4); qhnbrn : buffer std_logic_vector(4 to 4); ccodeoxfu : in real; hdb : inout bit);
end ontutmglde;

architecture thkkovn of ontutmglde is
  
begin
  -- Single-driven assignments
  qgarxanawa <= (others => ' ');
  hdb <= hdb;
  
  -- Multi-driven assignments
  qhnbrn <= (others => '0');
  qhnbrn <= (others => 'H');
  qhnbrn <= qhnbrn;
end thkkovn;

entity okixlua is
  port (igm : linkage integer);
end okixlua;

library ieee;
use ieee.std_logic_1164.all;

architecture wctrs of okixlua is
  signal flh : bit;
  signal cphdiv : real;
  signal lwjnsitklf : std_logic_vector(4 to 4);
  signal fahvrxf : string(3 downto 4);
  signal vw : bit;
  signal ayplu : std_logic_vector(4 to 4);
  signal elzcfxxrt : string(3 downto 4);
  signal qto : bit;
  signal ipbq : real;
  signal e : std_logic_vector(4 to 4);
  signal kdu : string(3 downto 4);
begin
  bfc : entity work.ontutmglde
    port map (qgarxanawa => kdu, qhnbrn => e, ccodeoxfu => ipbq, hdb => qto);
  wlteg : entity work.ontutmglde
    port map (qgarxanawa => elzcfxxrt, qhnbrn => ayplu, ccodeoxfu => ipbq, hdb => vw);
  q : entity work.ontutmglde
    port map (qgarxanawa => fahvrxf, qhnbrn => lwjnsitklf, ccodeoxfu => cphdiv, hdb => flh);
  
  -- Single-driven assignments
  ipbq <= ipbq;
  cphdiv <= 2#0.0#;
  
  -- Multi-driven assignments
  e <= e;
  e <= "Z";
  e <= "L";
end wctrs;



-- Seed after: 14734141531404350464,662889661651915549
