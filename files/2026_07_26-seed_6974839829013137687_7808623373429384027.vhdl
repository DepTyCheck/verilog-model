-- Seed: 6974839829013137687,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity xrncujvmdd is
  port (qchlg : linkage integer; kfspcoy : buffer time; pjn : linkage std_logic);
end xrncujvmdd;

architecture ytdco of xrncujvmdd is
  
begin
  -- Single-driven assignments
  kfspcoy <= kfspcoy;
end ytdco;

entity gxwvqcj is
  port (mibfn : buffer time);
end gxwvqcj;

library ieee;
use ieee.std_logic_1164.all;

architecture jmdxxoc of gxwvqcj is
  signal mjozmip : integer;
  signal jgfoj : time;
  signal txq : integer;
  signal ewadt : std_logic;
  signal pwy : time;
  signal czu : integer;
  signal hfq : std_logic;
  signal jowtmzzdup : time;
  signal rcggaftwlp : integer;
begin
  uypqiht : entity work.xrncujvmdd
    port map (qchlg => rcggaftwlp, kfspcoy => jowtmzzdup, pjn => hfq);
  eoj : entity work.xrncujvmdd
    port map (qchlg => czu, kfspcoy => pwy, pjn => ewadt);
  ceusae : entity work.xrncujvmdd
    port map (qchlg => txq, kfspcoy => jgfoj, pjn => hfq);
  ngjydjrr : entity work.xrncujvmdd
    port map (qchlg => mjozmip, kfspcoy => mibfn, pjn => hfq);
  
  -- Multi-driven assignments
  ewadt <= hfq;
  hfq <= 'W';
  ewadt <= hfq;
end jmdxxoc;

entity bymzdb is
  port (cybo : linkage real; wr : inout integer);
end bymzdb;

library ieee;
use ieee.std_logic_1164.all;

architecture dgqbk of bymzdb is
  signal qujnaapuk : std_logic;
  signal omqzsuod : time;
  signal eeqbsy : integer;
  signal ljnydf : std_logic;
  signal dpm : time;
  signal fr : integer;
begin
  u : entity work.xrncujvmdd
    port map (qchlg => fr, kfspcoy => dpm, pjn => ljnydf);
  l : entity work.xrncujvmdd
    port map (qchlg => eeqbsy, kfspcoy => omqzsuod, pjn => qujnaapuk);
  
  -- Single-driven assignments
  wr <= eeqbsy;
  
  -- Multi-driven assignments
  ljnydf <= ljnydf;
  ljnydf <= ljnydf;
  ljnydf <= 'X';
end dgqbk;

library ieee;
use ieee.std_logic_1164.all;

entity kyhxklfifg is
  port (lw : in std_logic_vector(2 to 2); qudxg : inout time; ycisuvbhvg : out std_logic_vector(1 downto 2));
end kyhxklfifg;

library ieee;
use ieee.std_logic_1164.all;

architecture chpkrjx of kyhxklfifg is
  signal hirdeso : time;
  signal rw : integer;
  signal z : integer;
  signal se : real;
  signal gtfrsprxo : std_logic;
  signal tfjd : time;
  signal oviytirdy : integer;
begin
  elgjyc : entity work.xrncujvmdd
    port map (qchlg => oviytirdy, kfspcoy => tfjd, pjn => gtfrsprxo);
  nlib : entity work.bymzdb
    port map (cybo => se, wr => z);
  mbexs : entity work.xrncujvmdd
    port map (qchlg => rw, kfspcoy => hirdeso, pjn => gtfrsprxo);
  bxqaklm : entity work.gxwvqcj
    port map (mibfn => qudxg);
  
  -- Multi-driven assignments
  ycisuvbhvg <= "";
  ycisuvbhvg <= "";
end chpkrjx;



-- Seed after: 12660213346494884817,7808623373429384027
