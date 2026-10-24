-- Seed: 8476749439376649626,10975587770517081373

entity ktatjzru is
  port (puagywxziy : linkage boolean_vector(1 downto 0); srfitb : buffer bit; miqejds : inout time);
end ktatjzru;

architecture jxumbv of ktatjzru is
  
begin
  -- Single-driven assignments
  miqejds <= 8#5_5# ns;
end jxumbv;

library ieee;
use ieee.std_logic_1164.all;

entity boalgxlx is
  port (t : inout std_logic_vector(0 downto 2));
end boalgxlx;

architecture ynzigsux of boalgxlx is
  signal kdtskwxl : time;
  signal lhtbic : bit;
  signal ruoiu : boolean_vector(1 downto 0);
begin
  djhosy : entity work.ktatjzru
    port map (puagywxziy => ruoiu, srfitb => lhtbic, miqejds => kdtskwxl);
  
  -- Multi-driven assignments
  t <= t;
  t <= (others => '0');
  t <= t;
end ynzigsux;

entity ijzbrnppfk is
  port (ukkhw : inout boolean; xp : inout integer);
end ijzbrnppfk;

library ieee;
use ieee.std_logic_1164.all;

architecture qcpo of ijzbrnppfk is
  signal lkrenxc : time;
  signal qxljeid : bit;
  signal a : boolean_vector(1 downto 0);
  signal jbulamv : std_logic_vector(0 downto 2);
  signal imnplpimfy : time;
  signal zfcrht : bit;
  signal cizelmwzc : boolean_vector(1 downto 0);
  signal qc : time;
  signal dlhk : bit;
  signal ohjj : boolean_vector(1 downto 0);
begin
  oqtbk : entity work.ktatjzru
    port map (puagywxziy => ohjj, srfitb => dlhk, miqejds => qc);
  hhatnl : entity work.ktatjzru
    port map (puagywxziy => cizelmwzc, srfitb => zfcrht, miqejds => imnplpimfy);
  ionk : entity work.boalgxlx
    port map (t => jbulamv);
  vwhr : entity work.ktatjzru
    port map (puagywxziy => a, srfitb => qxljeid, miqejds => lkrenxc);
  
  -- Single-driven assignments
  xp <= 1_1_4;
  ukkhw <= FALSE;
  
  -- Multi-driven assignments
  jbulamv <= (others => '0');
  jbulamv <= jbulamv;
  jbulamv <= jbulamv;
end qcpo;



-- Seed after: 10735395011070866962,10975587770517081373
