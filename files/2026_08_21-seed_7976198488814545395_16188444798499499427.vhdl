-- Seed: 7976198488814545395,16188444798499499427

entity kxgooi is
  port (wt : out time; uykohof : in boolean);
end kxgooi;

architecture zq of kxgooi is
  
begin
  -- Single-driven assignments
  wt <= wt;
end zq;

library ieee;
use ieee.std_logic_1164.all;

entity idxezhfx is
  port (akzzd : inout std_logic; xspmzlwya : in std_logic_vector(4 to 4); s : buffer std_logic; bybqpak : out std_logic);
end idxezhfx;

architecture bqk of idxezhfx is
  signal exfvm : boolean;
  signal nqkaegf : time;
  signal wnagehtiey : time;
  signal hkotqbdtxe : time;
  signal yofcxfqc : boolean;
  signal dekpvtuofd : time;
begin
  kxbbssy : entity work.kxgooi
    port map (wt => dekpvtuofd, uykohof => yofcxfqc);
  qqzrdbpilt : entity work.kxgooi
    port map (wt => hkotqbdtxe, uykohof => yofcxfqc);
  oem : entity work.kxgooi
    port map (wt => wnagehtiey, uykohof => yofcxfqc);
  h : entity work.kxgooi
    port map (wt => nqkaegf, uykohof => exfvm);
  
  -- Single-driven assignments
  yofcxfqc <= FALSE;
  
  -- Multi-driven assignments
  s <= 'L';
  bybqpak <= '-';
  bybqpak <= 'W';
  bybqpak <= 'H';
end bqk;

library ieee;
use ieee.std_logic_1164.all;

entity dljmaxc is
  port (aejlzo : inout std_logic_vector(3 to 0); djtcjoqrj : in std_logic; spa : in severity_level; imgwwy : inout time_vector(4 downto 1));
end dljmaxc;

architecture dgmjdyo of dljmaxc is
  signal hetvanli : time;
  signal iif : boolean;
  signal f : time;
begin
  zsntetgkz : entity work.kxgooi
    port map (wt => f, uykohof => iif);
  oyqo : entity work.kxgooi
    port map (wt => hetvanli, uykohof => iif);
  
  -- Multi-driven assignments
  aejlzo <= aejlzo;
  aejlzo <= "";
  aejlzo <= aejlzo;
end dgmjdyo;

library ieee;
use ieee.std_logic_1164.all;

entity wiq is
  port (b : buffer time; yaases : out time; wvdlza : linkage std_logic);
end wiq;

library ieee;
use ieee.std_logic_1164.all;

architecture nsusdofmqp of wiq is
  signal frfrlyagmb : boolean;
  signal ejmnb : time;
  signal xhjsxecwj : std_logic;
  signal rpue : std_logic;
  signal cx : std_logic;
  signal feifigv : std_logic_vector(4 to 4);
  signal jzbacrhll : std_logic;
  signal ehkyaeiftt : time_vector(4 downto 1);
  signal xnsikd : severity_level;
  signal fuxeyk : std_logic;
  signal ireucri : std_logic_vector(3 to 0);
begin
  gczfyg : entity work.dljmaxc
    port map (aejlzo => ireucri, djtcjoqrj => fuxeyk, spa => xnsikd, imgwwy => ehkyaeiftt);
  fgyh : entity work.idxezhfx
    port map (akzzd => jzbacrhll, xspmzlwya => feifigv, s => jzbacrhll, bybqpak => fuxeyk);
  krqvrodnkh : entity work.idxezhfx
    port map (akzzd => cx, xspmzlwya => feifigv, s => rpue, bybqpak => xhjsxecwj);
  egyv : entity work.kxgooi
    port map (wt => ejmnb, uykohof => frfrlyagmb);
  
  -- Single-driven assignments
  yaases <= 8#1_0_6_7_4.2344# ps;
  
  -- Multi-driven assignments
  xhjsxecwj <= 'H';
  jzbacrhll <= 'W';
end nsusdofmqp;



-- Seed after: 11240837547647772694,16188444798499499427
