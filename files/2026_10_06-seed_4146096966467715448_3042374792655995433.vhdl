-- Seed: 4146096966467715448,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity vdpnf is
  port (zhngvh : buffer std_logic; mjwbpvblqt : in time);
end vdpnf;

architecture rkqikcmk of vdpnf is
  
begin
  -- Multi-driven assignments
  zhngvh <= '0';
  zhngvh <= 'L';
  zhngvh <= zhngvh;
  zhngvh <= 'X';
end rkqikcmk;

entity olnzpmf is
  port (fq : linkage time);
end olnzpmf;

library ieee;
use ieee.std_logic_1164.all;

architecture siihf of olnzpmf is
  signal e : std_logic;
  signal h : std_logic;
  signal rqcsmkmsp : std_logic;
  signal imq : time;
  signal umrl : std_logic;
begin
  ctsigeyed : entity work.vdpnf
    port map (zhngvh => umrl, mjwbpvblqt => imq);
  nqyuio : entity work.vdpnf
    port map (zhngvh => rqcsmkmsp, mjwbpvblqt => imq);
  wzrxhx : entity work.vdpnf
    port map (zhngvh => h, mjwbpvblqt => imq);
  uflvuicoch : entity work.vdpnf
    port map (zhngvh => e, mjwbpvblqt => imq);
  
  -- Multi-driven assignments
  umrl <= 'X';
  umrl <= umrl;
  e <= rqcsmkmsp;
  umrl <= umrl;
end siihf;

entity p is
  port (gcricrkyy : in integer; qfgjzcsm : buffer integer; ognhbie : in time);
end p;

architecture xzefch of p is
  signal a : time;
  signal gwsfw : time;
begin
  doa : entity work.olnzpmf
    port map (fq => gwsfw);
  y : entity work.olnzpmf
    port map (fq => a);
  
  -- Single-driven assignments
  qfgjzcsm <= qfgjzcsm;
end xzefch;



-- Seed after: 5132693432788393342,3042374792655995433
