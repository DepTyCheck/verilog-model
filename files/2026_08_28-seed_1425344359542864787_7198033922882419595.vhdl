-- Seed: 1425344359542864787,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity vhyll is
  port (wwc : inout string(3 to 3); dmwgvitwe : linkage std_logic_vector(3 to 4); qzewhczm : in integer);
end vhyll;

architecture koiqkpn of vhyll is
  
begin
  
end koiqkpn;

entity zvlcodkkh is
  port (bfrslhjl : linkage bit; besi : buffer bit);
end zvlcodkkh;

library ieee;
use ieee.std_logic_1164.all;

architecture dmrleul of zvlcodkkh is
  signal hsw : integer;
  signal evyyeqy : string(3 to 3);
  signal kjvztljza : integer;
  signal awusxb : string(3 to 3);
  signal ka : integer;
  signal khnz : string(3 to 3);
  signal jnqcgzguub : integer;
  signal popwortvd : std_logic_vector(3 to 4);
  signal evecxytna : string(3 to 3);
begin
  j : entity work.vhyll
    port map (wwc => evecxytna, dmwgvitwe => popwortvd, qzewhczm => jnqcgzguub);
  xctffshxo : entity work.vhyll
    port map (wwc => khnz, dmwgvitwe => popwortvd, qzewhczm => ka);
  lpxrjxlfs : entity work.vhyll
    port map (wwc => awusxb, dmwgvitwe => popwortvd, qzewhczm => kjvztljza);
  jcqtafo : entity work.vhyll
    port map (wwc => evyyeqy, dmwgvitwe => popwortvd, qzewhczm => hsw);
  
  -- Single-driven assignments
  kjvztljza <= jnqcgzguub;
  hsw <= 8#42#;
  
  -- Multi-driven assignments
  popwortvd <= "WW";
end dmrleul;

entity gaqgaombx is
  port (dkxczhwo : inout bit; kstyz : out integer);
end gaqgaombx;

architecture fw of gaqgaombx is
  signal tfbydx : bit;
  signal fkgnrka : bit;
  signal qir : bit;
begin
  vfmnorkkwg : entity work.zvlcodkkh
    port map (bfrslhjl => qir, besi => fkgnrka);
  cz : entity work.zvlcodkkh
    port map (bfrslhjl => dkxczhwo, besi => tfbydx);
end fw;



-- Seed after: 10492509968420949698,7198033922882419595
