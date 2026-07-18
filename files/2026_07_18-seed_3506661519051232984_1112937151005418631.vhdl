-- Seed: 3506661519051232984,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity vyjdnswg is
  port (ewptt : inout std_logic_vector(3 downto 3); qggxlpn : buffer integer);
end vyjdnswg;

architecture hxcbsylx of vyjdnswg is
  
begin
  -- Multi-driven assignments
  ewptt <= ewptt;
  ewptt <= "U";
  ewptt <= (others => 'X');
end hxcbsylx;

entity j is
  port (urtsrj : in bit_vector(1 to 4); nuzajjae : inout bit_vector(1 downto 4));
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture higwrndq of j is
  signal o : integer;
  signal kmdvxle : integer;
  signal mbbhiyjx : integer;
  signal nrj : std_logic_vector(3 downto 3);
  signal bslzd : integer;
  signal yf : std_logic_vector(3 downto 3);
begin
  xzlg : entity work.vyjdnswg
    port map (ewptt => yf, qggxlpn => bslzd);
  arigog : entity work.vyjdnswg
    port map (ewptt => nrj, qggxlpn => mbbhiyjx);
  hunlel : entity work.vyjdnswg
    port map (ewptt => nrj, qggxlpn => kmdvxle);
  tqbhxvqr : entity work.vyjdnswg
    port map (ewptt => yf, qggxlpn => o);
  
  -- Single-driven assignments
  nuzajjae <= (others => '0');
  
  -- Multi-driven assignments
  yf <= nrj;
  yf <= yf;
  yf <= yf;
  yf <= "1";
end higwrndq;

entity wbaq is
  port (gdqbzlgnu : buffer integer);
end wbaq;

library ieee;
use ieee.std_logic_1164.all;

architecture gcncnvd of wbaq is
  signal vpsgqkii : integer;
  signal eknvem : integer;
  signal hj : integer;
  signal fykpy : std_logic_vector(3 downto 3);
begin
  ehuxcjfva : entity work.vyjdnswg
    port map (ewptt => fykpy, qggxlpn => hj);
  xgls : entity work.vyjdnswg
    port map (ewptt => fykpy, qggxlpn => eknvem);
  jojnkklr : entity work.vyjdnswg
    port map (ewptt => fykpy, qggxlpn => vpsgqkii);
  
  -- Single-driven assignments
  gdqbzlgnu <= gdqbzlgnu;
end gcncnvd;



-- Seed after: 8757112134319189242,1112937151005418631
