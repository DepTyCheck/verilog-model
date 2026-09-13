-- Seed: 3196518590039434777,10754487200446211253

entity kmlhchsz is
  port (lgwchrdo : linkage integer; zq : inout time; nozwm : out integer);
end kmlhchsz;

architecture mmnzpig of kmlhchsz is
  
begin
  
end mmnzpig;

entity fap is
  port (lnfdmye : buffer real);
end fap;

architecture tzsuubijf of fap is
  signal sk : integer;
  signal edo : time;
  signal rvlmtesuj : integer;
  signal vsn : integer;
  signal wlx : time;
  signal nssemstktb : integer;
  signal bdcpcz : integer;
  signal jkxp : time;
  signal omhi : integer;
  signal umsdjs : integer;
  signal lkla : time;
  signal suf : integer;
begin
  bvchicvd : entity work.kmlhchsz
    port map (lgwchrdo => suf, zq => lkla, nozwm => umsdjs);
  crggpbn : entity work.kmlhchsz
    port map (lgwchrdo => omhi, zq => jkxp, nozwm => bdcpcz);
  nq : entity work.kmlhchsz
    port map (lgwchrdo => nssemstktb, zq => wlx, nozwm => vsn);
  u : entity work.kmlhchsz
    port map (lgwchrdo => rvlmtesuj, zq => edo, nozwm => sk);
  
  -- Single-driven assignments
  lnfdmye <= lnfdmye;
end tzsuubijf;

library ieee;
use ieee.std_logic_1164.all;

entity opftuineh is
  port (n : out time_vector(0 downto 3); l : out std_logic; alh : inout std_logic_vector(3 downto 3); yje : linkage std_logic_vector(1 downto 3));
end opftuineh;

architecture dshcfgploo of opftuineh is
  signal wginpoewh : integer;
  signal qw : time;
  signal mpoibdn : integer;
begin
  umb : entity work.kmlhchsz
    port map (lgwchrdo => mpoibdn, zq => qw, nozwm => wginpoewh);
  
  -- Single-driven assignments
  n <= (others => 0 ns);
  
  -- Multi-driven assignments
  alh <= alh;
end dshcfgploo;



-- Seed after: 14663657501469972045,10754487200446211253
