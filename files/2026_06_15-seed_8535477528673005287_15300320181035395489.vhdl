-- Seed: 8535477528673005287,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity gxiklettby is
  port (mqeeoaf : linkage std_logic; n : linkage bit; ikh : out time_vector(1 downto 2); toesztfx : out std_logic_vector(4 to 4));
end gxiklettby;

architecture x of gxiklettby is
  
begin
  -- Single-driven assignments
  ikh <= (others => 0 ns);
  
  -- Multi-driven assignments
  toesztfx <= "W";
  toesztfx <= (others => 'U');
  toesztfx <= (others => 'H');
end x;

entity svj is
  port (cqzhe : linkage time; sbvqleag : in integer; qcsdjvp : buffer integer; reky : inout bit);
end svj;

library ieee;
use ieee.std_logic_1164.all;

architecture jcills of svj is
  signal adsumzupst : time_vector(1 downto 2);
  signal affadyuym : std_logic;
  signal edanmuo : std_logic_vector(4 to 4);
  signal pjdnmzkkh : time_vector(1 downto 2);
  signal sojvlqxf : bit;
  signal qfcyuptlsq : std_logic;
  signal jsfaqr : std_logic_vector(4 to 4);
  signal id : time_vector(1 downto 2);
  signal bwgmm : bit;
  signal gpxet : std_logic_vector(4 to 4);
  signal eiyv : time_vector(1 downto 2);
  signal uv : bit;
  signal kzwsjw : std_logic;
begin
  bsfxrot : entity work.gxiklettby
    port map (mqeeoaf => kzwsjw, n => uv, ikh => eiyv, toesztfx => gpxet);
  rbdfuzchzv : entity work.gxiklettby
    port map (mqeeoaf => kzwsjw, n => bwgmm, ikh => id, toesztfx => jsfaqr);
  llmlokwvni : entity work.gxiklettby
    port map (mqeeoaf => qfcyuptlsq, n => sojvlqxf, ikh => pjdnmzkkh, toesztfx => edanmuo);
  wkpdmrrl : entity work.gxiklettby
    port map (mqeeoaf => affadyuym, n => reky, ikh => adsumzupst, toesztfx => gpxet);
  
  -- Single-driven assignments
  qcsdjvp <= 2;
  
  -- Multi-driven assignments
  affadyuym <= '0';
end jcills;

entity w is
  port (zkairtqu : linkage bit_vector(1 downto 3); tuykjdoitl : buffer severity_level; xeyhtt : in string(1 downto 2); xw : in time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture otggluzi of w is
  signal dwdbf : time_vector(1 downto 2);
  signal nxyphmo : bit;
  signal lcfm : time_vector(1 downto 2);
  signal znjyzsbwx : bit;
  signal oebwt : std_logic;
  signal vl : std_logic_vector(4 to 4);
  signal uxdn : time_vector(1 downto 2);
  signal d : bit;
  signal wcnd : std_logic;
  signal ysslukvx : std_logic_vector(4 to 4);
  signal jrkvxq : time_vector(1 downto 2);
  signal pytrhpuz : bit;
  signal mdasakcc : std_logic;
begin
  uemfrgtqt : entity work.gxiklettby
    port map (mqeeoaf => mdasakcc, n => pytrhpuz, ikh => jrkvxq, toesztfx => ysslukvx);
  xir : entity work.gxiklettby
    port map (mqeeoaf => wcnd, n => d, ikh => uxdn, toesztfx => vl);
  syvydricfn : entity work.gxiklettby
    port map (mqeeoaf => oebwt, n => znjyzsbwx, ikh => lcfm, toesztfx => vl);
  redlbfhg : entity work.gxiklettby
    port map (mqeeoaf => mdasakcc, n => nxyphmo, ikh => dwdbf, toesztfx => ysslukvx);
end otggluzi;

entity drd is
  port (vhbsazynh : out real_vector(2 downto 1));
end drd;

library ieee;
use ieee.std_logic_1164.all;

architecture a of drd is
  signal fk : time_vector(1 downto 2);
  signal tzh : bit;
  signal u : bit;
  signal cwsiqgnyg : integer;
  signal fpgundr : integer;
  signal ectkcszyd : time;
  signal ijch : std_logic_vector(4 to 4);
  signal goig : time_vector(1 downto 2);
  signal oqjorkfwyf : bit;
  signal hydldzao : std_logic;
begin
  cztah : entity work.gxiklettby
    port map (mqeeoaf => hydldzao, n => oqjorkfwyf, ikh => goig, toesztfx => ijch);
  cgwiq : entity work.svj
    port map (cqzhe => ectkcszyd, sbvqleag => fpgundr, qcsdjvp => cwsiqgnyg, reky => u);
  mnrcmxqhik : entity work.gxiklettby
    port map (mqeeoaf => hydldzao, n => tzh, ikh => fk, toesztfx => ijch);
  
  -- Single-driven assignments
  fpgundr <= 8#7_4_7_0_3#;
  vhbsazynh <= (40230.11024, 2#01110.110#);
  
  -- Multi-driven assignments
  hydldzao <= 'U';
end a;



-- Seed after: 11467403303475852806,15300320181035395489
