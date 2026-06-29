-- Seed: 14180463416048479871,17047277710231705797

entity e is
  port (noc : buffer string(2 downto 3); rcp : linkage real);
end e;

architecture dhkcobc of e is
  
begin
  -- Single-driven assignments
  noc <= (others => ' ');
end dhkcobc;

entity guqbpvbxpn is
  port (cxohrutnfa : buffer real; vujvlyc : out real; ktzig : buffer bit);
end guqbpvbxpn;

architecture nqexm of guqbpvbxpn is
  signal wirrvebzj : real;
  signal kqxfgzqzf : string(2 downto 3);
  signal qhyhoq : string(2 downto 3);
begin
  uqaezr : entity work.e
    port map (noc => qhyhoq, rcp => vujvlyc);
  cknrbyor : entity work.e
    port map (noc => kqxfgzqzf, rcp => wirrvebzj);
  
  -- Single-driven assignments
  ktzig <= '0';
  cxohrutnfa <= 16#1_7.1_9_A_6_9#;
end nqexm;

library ieee;
use ieee.std_logic_1164.all;

entity lbmsfkgaa is
  port (j : linkage character; tvzuezjh : linkage std_logic_vector(1 to 3); ixgex : out std_logic; cnv : in boolean);
end lbmsfkgaa;

architecture bpp of lbmsfkgaa is
  signal omm : real;
  signal yubpg : string(2 downto 3);
  signal rtwqwphggx : real;
  signal ogiilbcw : string(2 downto 3);
begin
  zqjb : entity work.e
    port map (noc => ogiilbcw, rcp => rtwqwphggx);
  m : entity work.e
    port map (noc => yubpg, rcp => omm);
  
  -- Multi-driven assignments
  ixgex <= '-';
  ixgex <= '1';
end bpp;

entity miwcpb is
  port (nyrpwagfq : in time; bsnsg : in boolean_vector(0 downto 3); c : in integer; tj : inout integer);
end miwcpb;

library ieee;
use ieee.std_logic_1164.all;

architecture ce of miwcpb is
  signal w : real;
  signal jrkqwbhy : string(2 downto 3);
  signal sjflucb : real;
  signal fnxucnhm : string(2 downto 3);
  signal qr : real;
  signal kp : string(2 downto 3);
  signal wn : boolean;
  signal wwqkz : std_logic;
  signal pwdfpmljf : std_logic_vector(1 to 3);
  signal mzefepnmcy : character;
begin
  uhnjoaxkzi : entity work.lbmsfkgaa
    port map (j => mzefepnmcy, tvzuezjh => pwdfpmljf, ixgex => wwqkz, cnv => wn);
  nidmatzhiw : entity work.e
    port map (noc => kp, rcp => qr);
  rsspqoss : entity work.e
    port map (noc => fnxucnhm, rcp => sjflucb);
  fuobureyeh : entity work.e
    port map (noc => jrkqwbhy, rcp => w);
  
  -- Single-driven assignments
  wn <= FALSE;
  tj <= 8#3316#;
  
  -- Multi-driven assignments
  pwdfpmljf <= "W0Z";
  pwdfpmljf <= ('0', 'W', '0');
  pwdfpmljf <= ('1', 'W', 'W');
end ce;



-- Seed after: 9163044113784795852,17047277710231705797
