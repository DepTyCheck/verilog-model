-- Seed: 14795406595964517379,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity kgffolm is
  port (awhyjsh : buffer integer; dqzxwrw : out integer; ngwnkrly : buffer time; wt : in std_logic_vector(4 downto 4));
end kgffolm;

architecture ifvh of kgffolm is
  
begin
  -- Single-driven assignments
  ngwnkrly <= 2#0_0_0# ms;
  dqzxwrw <= 8#35#;
end ifvh;

library ieee;
use ieee.std_logic_1164.all;

entity nrdstw is
  port (yyrmbccvo : out std_logic_vector(1 downto 0); jd : linkage time);
end nrdstw;

library ieee;
use ieee.std_logic_1164.all;

architecture xcm of nrdstw is
  signal bzfxldyv : time;
  signal u : integer;
  signal ickgpa : integer;
  signal r : std_logic_vector(4 downto 4);
  signal y : time;
  signal hfqudptbnr : integer;
  signal eumad : integer;
  signal cti : time;
  signal cpmiljma : integer;
  signal jx : integer;
  signal hsrapp : std_logic_vector(4 downto 4);
  signal ta : time;
  signal tlpnph : integer;
  signal rjy : integer;
begin
  xtyop : entity work.kgffolm
    port map (awhyjsh => rjy, dqzxwrw => tlpnph, ngwnkrly => ta, wt => hsrapp);
  xrxcw : entity work.kgffolm
    port map (awhyjsh => jx, dqzxwrw => cpmiljma, ngwnkrly => cti, wt => hsrapp);
  husb : entity work.kgffolm
    port map (awhyjsh => eumad, dqzxwrw => hfqudptbnr, ngwnkrly => y, wt => r);
  ogqnnlo : entity work.kgffolm
    port map (awhyjsh => ickgpa, dqzxwrw => u, ngwnkrly => bzfxldyv, wt => hsrapp);
  
  -- Multi-driven assignments
  r <= (others => 'X');
end xcm;

library ieee;
use ieee.std_logic_1164.all;

entity oitc is
  port (rijci : buffer real_vector(2 to 2); emielpsj : buffer bit_vector(1 downto 3); sm : out std_logic_vector(1 downto 0));
end oitc;

library ieee;
use ieee.std_logic_1164.all;

architecture nyhtznur of oitc is
  signal x : std_logic_vector(4 downto 4);
  signal melctbcn : time;
  signal abaaza : integer;
  signal wudt : integer;
  signal vjbgxlfv : time;
  signal mxhbzrcd : std_logic_vector(1 downto 0);
  signal aiivk : std_logic_vector(4 downto 4);
  signal zhmiigfpaz : time;
  signal ibtyqc : integer;
  signal cvoesofr : integer;
  signal scm : std_logic_vector(4 downto 4);
  signal n : time;
  signal etvoqy : integer;
  signal byvngjphb : integer;
begin
  cxnfq : entity work.kgffolm
    port map (awhyjsh => byvngjphb, dqzxwrw => etvoqy, ngwnkrly => n, wt => scm);
  mflposvpmq : entity work.kgffolm
    port map (awhyjsh => cvoesofr, dqzxwrw => ibtyqc, ngwnkrly => zhmiigfpaz, wt => aiivk);
  ihgrtdm : entity work.nrdstw
    port map (yyrmbccvo => mxhbzrcd, jd => vjbgxlfv);
  phpaetnfnv : entity work.kgffolm
    port map (awhyjsh => wudt, dqzxwrw => abaaza, ngwnkrly => melctbcn, wt => x);
  
  -- Multi-driven assignments
  mxhbzrcd <= ('Z', 'Z');
  x <= (others => 'L');
  sm <= "0H";
end nyhtznur;

library ieee;
use ieee.std_logic_1164.all;

entity ss is
  port (lshaijloa : out boolean; nzkezx : in std_logic; ycwki : buffer std_logic; nggp : linkage integer);
end ss;

architecture jhtskoz of ss is
  
begin
  -- Multi-driven assignments
  ycwki <= 'Z';
  ycwki <= '-';
  ycwki <= 'X';
end jhtskoz;



-- Seed after: 4131782371772356892,15300320181035395489
